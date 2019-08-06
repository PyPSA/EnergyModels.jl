module PyPSANetwork

using AxisArrays

using PyCall
import Pandas
const PD = Pandas
# const PD = Base.require(Base.PkgId(Base.UUID("eadc2687-ae89-51f9-a5d9-86b5a6373a9c"), "Pandas"))

using DataFrames
using Dates

using ..EnergyModels
const EM = EnergyModels

using ..EnergyModels:
    AbstractData, Data, pushaxes!, resolve, @consense, Component, Device,
    DeviceFormulation, attributes, components, naming, astype, typenames

using JuMP: value, dual, ConstraintRef, VariableRef, GenericAffExpr

export PypsaNetworkData

function EM.load(network::PyObject)::AbstractData
    pypsa = pyimport("pypsa")
    if pyisinstance(network, pypsa.Network)
        Data(fallback=PypsaNetworkData(network))
    else
        error("Python object is no PyPSA Network")
    end
end

abstract type AbstractAttrDesc end

struct AttrDesc{T,S} <: AbstractAttrDesc
    data::S
    indices::Union{Colon,UnitRange{Int64},StepRange{Int64,Int64},Vector{Int64}}
    default::Union{T,Missing}

    AttrDesc{T}(data::S, indices, default) where {T,S} = new{T,S}(data, indices, default)
end

struct SingleAttrDesc{T} <: AbstractAttrDesc
    value::T
end

struct ComponentDesc
    name::Symbol
    axis::Axis
    componenttype
    attributes::Dict{Symbol,AbstractAttrDesc}
    typeparams::Union{Nothing,Dict{Symbol,Any}}
end

struct PypsaNetworkData <: AbstractData
    network::PyObject
    components::Dict{Symbol}{ComponentDesc}
    axes::Dict{Symbol}{Axis}

    function PypsaNetworkData(network, components)
        snapshots = PD.values(PD.Index(network.snapshots))
        if pyisinstance(network.snapshots, pyimport("pandas").DatetimeIndex)
            snapshots = DateTime.(snapshots)
        end
        axes = Dict(:snapshots => Axis{:snapshots}(snapshots),
                    (name => cd.axis for (name, cd) in components)...)
        new(network, components, axes)
    end
end

splitgroup(df, group) = [group]
function splitgroup(df, group, attr, attrs...)
    gdf = groupby(DataFrame(attr=>df[group.idx,attr], :idx=>group.idx, copycols=false), attr);

    if length(gdf) == 1
        ng = attr == :carrier ? (idx=group.idx, name=(group.name..., first(parent(gdf)[!,attr]))) : group
        groups = splitgroup(df, ng, attrs...)
    else
        groups = Any[]
        for sdf in gdf
            ng = (idx=collect(sdf.idx),
                  name=(group.name..., first(sdf[!,attr])))
            append!(groups, splitgroup(df, ng, attrs...))
        end
    end

    groups
end

function AttrDesc(::Type{Val{:single}}, attr, name, df, pnl, idx, axis)
    val = @consense(PD.values(df[attr[:PyPSA]])[idx],
                    "$(attr[:PyPSA]) for group $name must be a single value")
    SingleAttrDesc(astype(attr[:dtype], val))
end

AttrDesc(::Type{Val{:static}}, attr, name, df, pnl, idx, axis) =
    AttrDesc{typenames[attr[:dtype]]}(df[attr[:PyPSA]], idx, attr[:default])

function AttrDesc(::Type{Val{:series}}, attr, name, df, pnl, idx, axis)
    df_t = get(pnl, attr[:PyPSA], nothing)
    indices_t = !isnothing(df_t) ? findall(in(Symbol.(PD.values(PD.columns(df_t)))), axis) : []
    if length(indices_t) == 0
        AttrDesc(Val{:static}, attr, name, df, pnl, idx, axis)
    else
        @assert(length(indices_t) == length(axis),
                "$name has static and time-dependent $(attr[:PyPSA])")
        AttrDesc{typenames[attr[:dtype]]}(df_t, indices_t, attr[:default])
    end
end

attributedescriptions(name, df, pnl, idx, axis; attributes=nothing) =
    Dict(attr[:attribute] => AttrDesc(Val{attr[:dimensions]}, attr, name, df, pnl, idx, axis)
         for attr in eachrow(attributes)
         if attr[:PyPSA] in PD.values(PD.columns(df)))

function typeparamdescriptions(name, df, indices; types=nothing)
    isnothing(types) && return nothing
    typename = @consense(PD.values(df.type)[indices], "$name may not have more than one type")
    typ = types[types.name .== typename, :]
    Dict(c => typ[1, c] for c in names(typ) if c != :name)
end

"""
    PypsaNetworkData

"""
function PypsaNetworkData(net)
    # Determine defined components and split them into classes
    components = vcat(
        getcomponents(net, :buses),
        getcomponents(net, :generators, withname=false, exp=:p_nom_extendable, uc=:committable),
        getcomponents(net, :storageunits, withname=false, pypsaname=:storage_units, exp=:p_nom_extendable),
        getcomponents(net, :stores, carrier=:bus, exp=:e_nom_extendable),
        getcomponents(net, :loads, carrier=:bus),
        getcomponents(net, :links, carrier=(:bus0, :bus1), exp=:p_nom_extendable),
        getcomponents(net, :lines, carrier=:bus0, exp=:s_nom_extendable, types=:line_types),
        getcomponents(net, :transformers, carrier=:bus0, exp=:s_nom_extendable, types=:transformer_types)
    )

    compdict = Dict(cd.name => cd for cd in components)

    PypsaNetworkData(net, compdict)
end

function _maybe_as_range(x::AbstractArray{T,1}) where T<:Number
    if length(x) < 3 return Vector(x) end
    d = diff(x)
    s = d[1]
    if all(d .== s)
        s == 1 ? (x[1]:x[end]) : (x[1]:s:x[end])
    else
        Vector(x)
    end
end

_join(a...; delim="_") = join(a, delim)

function getcomponents(net, name; pypsaname=false, withname=true, carrier=true, exp=false, uc=false, types=nothing)
    if pypsaname === false
        pypsaname = name
    end

    df = PD.DataFrame(getproperty(net, pypsaname))
    pnl = Dict(k=>PD.DataFrame(v) for (k,v) in getproperty(net, Symbol(pypsaname, :_t)))

    components = ComponentDesc[]

    if isempty(df)
        return components
    end

    initialname = withname ? (name,) : ()

    attrs = DataFrame()
    if carrier === true
        attrs[!, :carrier] = PD.values(df.carrier)
    elseif carrier !== false
        buses_carrier = PD.Series(net.buses.carrier)
        attrs[!, :carrier] =
            if carrier isa Tuple
                _join.((map(buses_carrier, df[field]) for field in carrier)...)
            else
                PD.values(map(buses_carrier, df[carrier]))
            end
    end

    if exp !== false
        attrs[!, exp] = recode(Bool.(PD.values(df[exp])), false=>:fix, true=>:ext)
    end
    if uc !== false
        attrs[!, uc] = recode(Bool.(PD.values(df[uc])), false=>:lin, true=>:uc)
    end

    if !isnothing(types)
        types = DataFrame(PD.reset_index(PD.DataFrame(getproperty(net, types))))
    end

    componenttypeunion = resolve(Component, name)
    componentattributes = copy(attributes(componenttypeunion))
    componentattributes = componentattributes[componentattributes.quantitytype .!= "Variable", :]
    componentattributes[!, :PyPSA] = string.(componentattributes.attribute)
    componentattributes[!, :dimensions] = recode(length.(componentattributes.dimensions),
                                                 0=>:single, 1=>:static, 2=>:series,
                                                 3:20=>missing)

    names = PD.values(PD.index(df))
    for (timeattr, df) in pnl
        if !isempty(df)
            label = Symbol(:t_, timeattr)
            attrs[!, label] = recode(in.(names, Ref(PD.values(PD.columns(df)))),
                                     false=>missing, true=>label)
        end
    end

    groups =
        if isempty(attrs)
            [(idx=1:length(names),
              name=Symbol(join(skipmissing(initialname), "_")),
              attrs=Dict{Symbol}{Any}())]
        else
            [(idx=_maybe_as_range(group.idx),
              name=Symbol(join(skipmissing(group.name), "_")),
              attrs=Dict(pairs(attrs[first(group.idx),:])...))
             for group in splitgroup(attrs, (idx=1:size(attrs, 1), name=initialname), Base.names(attrs)...)]
        end

    for g in groups
        axis = Axis{g.name}(Symbol.(names[g.idx]))

        formulation = join(skipmissing([
            exp !== false ? ((coalesce(g.attrs[exp], :fix) == :ext) ? "linexp" : missing) : missing,
            uc !== false ?  ((coalesce(g.attrs[uc], :lin) == :uc)   ? "ucdisp" : "lindisp") : "lindisp"
        ]), "_")

        componenttype =
            if componenttypeunion <: Device && !isempty(formulation)
                componenttypeunion{resolve(DeviceFormulation, Symbol(formulation))}
            else
                componenttypeunion
            end

        attributes = attributedescriptions(g.name, df, pnl, g.idx, axis; attributes=componentattributes)
        typeparams = typeparamdescriptions(g.name, df, g.idx; types=types)

        push!(components, ComponentDesc(g.name, axis, componenttype, attributes, typeparams))
    end

    components
end

_value(a::AxisArray) = a
_value(a::AxisArray{<:ConstraintRef}) = AxisArray(dual.(a), AxisArrays.axes(a)...)
_value(a::AxisArray{<:VariableRef}) = AxisArray(value.(a), AxisArrays.axes(a)...)
_value(a::AxisArray{<:GenericAffExpr}) = AxisArray(value.(a), AxisArrays.axes(a)...)

function _set!(df, args...)
    @assert(length(args) % 2 == 0, "Needs an even number of arguments")
    arrays = map(_value, args[2:2:end])
    axes = AxisArrays.axes(first(arrays))
    factors = [EM.WrappedArray(f, axes...) for f in args[1:2:end]]
    agg = AxisArray(Array{Float64}(undef, size(first(arrays))), axes)
    for I in eachindex(IndexCartesian(), agg)
        agg[I] = sum(f[Tuple(I)...] * v[I] for (f,v) in zip(factors, arrays))
    end
    _set!(df, agg)
end

function _set!(ser::PD.Series, data)
    ax, = AxisArrays.axes(data)
    PD.loc(ser)[ax.val] = _value(data)
end

function _set!(df::PD.DataFrame, data)
    B, T = AxisArrays.axes(data)
    for (idx, b) in zip(PD.columns(df).pyo.get_indexer(B), B)
        if idx == -1
            df[b] = PD.Series(_value(data[b, :]), index=T.val)
        else
            PD.loc(df)[T.val, idx] = _value(data[b, :])
        end
    end
end

function _set!(df::PyObject, data)
    pd = pyimport("pandas")
    if pyisinstance(df, pd.DataFrame)
        _set!(PD.DataFrame(df), data)
    elseif pyisinstance(df, pd.Series)
        _set!(PD.Series(df), data)
    else
        error("Needs pandas series or dataframe")
    end
end

function store_results!(data::PypsaNetworkData, g::EM.Generator)
    _set!(data.network.generators_t["p"], g[:p])
    _set!(data.network.generators.p_nom_opt, g[:p_nom])
end

function store_results!(data::PypsaNetworkData, s::EM.StorageUnit)
    _set!(data.network.storage_units_t["p"], 1, s[:p_dispatch], -1, s[:p_store])
    _set!(data.network.storage_units_t["state_of_charge"], s[:state_of_charge])
    _set!(data.network.storage_units_t["spill"], s[:spill])
    _set!(data.network.storage_units.p_nom_opt, s[:p_nom])
end

function store_results!(data::PypsaNetworkData, s::EM.Store)
    _set!(data.network.stores_t["p"], s[:p])
    _set!(data.network.stores_t["e"], s[:e])
    _set!(data.network.stores.e_nom_opt, s[:e_nom])
end

function store_results!(data::PypsaNetworkData, l::EM.Load)
    _set!(data.network.loads_t["p"], transpose(l[:p_set]))
end

function store_results!(data::PypsaNetworkData, b::T) where T <: EM.PassiveBranch
    pnl = data.network.pnl(string(T.name))
    _set!(pnl["p0"], b[:p])
    _set!(pnl["p1"], -1, b[:p])
    _set!(pnl["mu_lower"], b[:p_lower])
    _set!(pnl["mu_upper"], -1, b[:p_upper])
    _set!(data.network.df(string(T.name)).s_nom_opt, b[:s_nom])
end

function store_results!(data::PypsaNetworkData, l::EM.Link)
    pnl = data.network.links_t
    _set!(pnl["p0"], l[:p])
    _set!(pnl["p1"], AxisArray(- l[:efficiency], AxisArrays.axes(l[:efficiency])...), l[:p])
    _set!(pnl["mu_lower"], l[:p_lower])
    _set!(pnl["mu_upper"], -1, l[:p_upper])
end

function store_results!(data::PypsaNetworkData, b::EM.Bus)
    pnl = data.network.buses_t
    _set!(pnl["marginal_price"], b[:p_balance])
    _set!(pnl["p"], EM.expression(EM.NodalActivePower(), EM.model(b), b))
    # TODO: v_ang and v_mag_pu needs to be calculated from B
end

function store_results!(data::PypsaNetworkData, c)
    @warn "Storing nothing for $(naming(c))"
end

function store_results!(data::PypsaNetworkData, model::EM.EnergyModel)
    map(b->store_results!(data, b), values(model.buses))
    for d in devices(model)
        store_results!(data, d)
    end
end

Base.get(::PypsaNetworkData, attrdesc::SingleAttrDesc, _) = attrdesc.value

Base.get(data::PypsaNetworkData, attrdesc::AttrDesc{Symbol,S}, ax) where S<:PD.Series =
    AxisArray(Symbol.(PD.values(attrdesc.data)[attrdesc.indices]), ax)

Base.get(data::PypsaNetworkData, attrdesc::AttrDesc{T,S}, ax) where {T, S<:PD.Series} =
    AxisArray(PD.values(attrdesc.data)[attrdesc.indices], ax)

Base.get(data::PypsaNetworkData, attrdesc::AttrDesc{T,S}, ax) where {T, S<:PD.DataFrame} =
    AxisArray(attrdesc.data.pyo.values[:,attrdesc.indices], data.axes[:snapshots], ax)

function Base.get(data::PypsaNetworkData, component::Component, param::Symbol)
    componentdesc = data.components[naming(component)]
    attrdesc = get(componentdesc.attributes, param, nothing)
    !isnothing(attrdesc) ? get(data, componentdesc.attributes[param], componentdesc.axis) : nothing
end

EM.gettypeparams(data::PypsaNetworkData, device::Device) =
    data.components[naming(device)].typeparams

EM.axis(data::PypsaNetworkData, c::Component) = data.axes[naming(c)]
EM.axis(data::PypsaNetworkData, name::Symbol) = data.axes[name]

EM.components(data::PypsaNetworkData) = EM._components(data)

end
