module PyPSAData

using AxisArrays
using NCDatasets
using DataFrames
using CSV
using Base.Iterators: flatten

using ..EnergyModels
const EM = EnergyModels

using ..EnergyModels:
    AbstractNcData, pushaxes!, resolve, @consense, Component, Device,
    DeviceFormulation, attributes, components, naming, astype, typenames

export PypsaNcData

abstract type AbstractAttrDesc end

struct AttrDesc{T} <: AbstractAttrDesc
    pypsaname::String
    indices::Union{Colon,UnitRange{Int64},StepRange{Int64,Int64},Vector{Int64}}
    default::Union{T,Missing}
end

struct SingleAttrDesc{T} <: AbstractAttrDesc
    value::T
end

struct MissingAttrDesc{T} <: AbstractAttrDesc
    default::T
end

struct ComponentDesc
    name::Symbol
    axis::Axis
    componenttype
    attributes::Dict{Symbol,AbstractAttrDesc}
    typeparams::Union{Nothing,Dict{Symbol,Any}}
end

struct PypsaNcData <: AbstractNcData
    dataset::Dataset
    components::Dict{Symbol}{ComponentDesc}
    axes::Dict{Symbol}{Axis}

    function PypsaNcData(dataset, components)
        data = new(dataset, components, Dict{Symbol}{Axis}())
        for cd in values(data.components)
            pushaxes!(data, cd)
        end
        data
    end
end

"""
    _dimnames(::PypsaNcData, ::AbstractAttrDesc)

returns tuples for the dimensions EXCEPT for the main component axis.
"""
_dimnames(attrdesc::AbstractAttrDesc; ds=nothing) = ()
_dimnames(attrdesc::AttrDesc; ds=nothing) = dimnames(ds[attrdesc.pypsaname])[2:end]

function EM.pushaxes!(data::PypsaNcData, cd::ComponentDesc)
    pushaxes!(data, cd.axis, component=cd.name, attribute="primary")

    dims = unique(flatten((_dimnames(attrdesc; ds=data.dataset) for attrdesc in values(cd.attributes))))
    for dim in dims
        pushaxes!(data, Axis{Symbol(dim)}(nomissing(data.dataset[dim][:])), component=cd.name, attribute=dim)
    end
end

function _getattr(ds, pypsaname, attr)
    fullname = string(pypsaname, "_", attr)
    haskey(ds, fullname) ? nomissing(ds[fullname][:]) : Array{Union{Float64,Missing}}(missing, length(ds[string(pypsaname, "_i")]))
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

AttrDesc(::Type{Val{:single}}, attr, name, ds, attrname, attrname_t, idx, axis) =
    SingleAttrDesc(
        haskey(ds, attrname) ?
        astype(attr[:dtype], @consense(ds[attrname][:][idx], "$name must be a single value")) :
        attr[:default]
    )

function AttrDesc(::Type{Val{:static}}, attr, name, ds, attrname, attrname_t, idx, axis)
    haskey(ds, attrname) ?
        AttrDesc{typenames[attr[:dtype]]}(attrname, idx, attr[:default]) :
        MissingAttrDesc(attr[:default])
end

function AttrDesc(::Type{Val{:series}}, attr, name, ds, attrname, attrname_t, idx, axis)
    attrname_t_i = attrname_t * "_i"
    indices_t = haskey(ds, attrname_t_i) ? findall(in(Symbol.(ds[attrname_t_i][:])), axis) : []
    if length(indices_t) == 0
        AttrDesc(Val{:static}, attr, name, ds, attrname, attrname_t, idx, axis)
    else
        @assert(length(indices_t) == length(axis),
                "$name has static and time-dependent $(attr[:PyPSA])")
        AttrDesc(attrname_t, indices_t, attr[:default])
    end
end

AttrDesc(attr, name, ds, pypsaname, idx, axis) =
    AttrDesc(Val{attr[:dimensions]}, attr, name, ds,
             string(pypsaname, "_", attr[:PyPSA]),
             string(pypsaname, "_t_", attr[:PyPSA]),
             idx, axis)

attributedescriptions(args...; attributes=nothing) =
    Dict(attr[:attribute]=>AttrDesc(attr, args...) for attr = eachrow(attributes))

function typeparamdescriptions(name, ds, listname, indices; types=nothing)
    typefield = string(listname, "_type")
    if types === nothing || !haskey(ds, typefield) return nothing end
    typename = @consense(ds[typefield][:][indices], "$name may not have more than one type")
    typ = types[types.name .== typename, :]
    Dict(c => typ[1, c] for c = names(typ) if c != :name)
end

"""
    PypsaNcData

"""
function PypsaNcData(ds)
    # Determine defined components and split them into classes
    components = vcat(
        getcomponents(ds, :buses),
        getcomponents(ds, :generators, withname=false, exp=:p_nom_extendable, uc=:committable),
        getcomponents(ds, :storageunits, withname=false, pypsaname="storage_units", exp=:p_nom_extendable),
        getcomponents(ds, :stores, carrier=:bus, exp=:e_nom_extendable),
        getcomponents(ds, :loads, carrier=:bus),
        getcomponents(ds, :links, carrier=(:bus0, :bus1), exp=:p_nom_extendable),
        getcomponents(ds, :lines, carrier=:bus0, exp=:s_nom_extendable),
        getcomponents(ds, :transformers, carrier=:bus0, exp=:s_nom_extendable)
    )

    compdict = Dict(cd.name => cd for cd in components)
    PypsaNcData(ds, compdict)
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
getcarrier(ds, buses_carrier, pypsaname, busattr) =
    getindex.(buses_carrier, nomissing(ds[string(pypsaname, "_", busattr)]))
getcarrier(ds, buses_carrier, pypsaname, busattrs::Tuple) =
    _join.((getcarrier(ds, buses_carrier, pypsaname, busattr) for busattr in busattrs)...)
function getcarrier(ds, pypsaname, busattrs)
    buses_carrier = Dict(nomissing(ds["buses_i"][:]) .=> nomissing(ds["buses_carrier"][:]))
    getcarrier(ds, buses_carrier, pypsaname, busattrs)
end

_length(carrier) = 1
_length(carrier::Tuple) = length(carrier)

function getcomponents(ds, name; pypsaname=false, withname=true, carrier=true, exp=false, uc=false)
    if pypsaname === false
        pypsaname = string(name)
    end

    components = ComponentDesc[]

    if !haskey(ds.dim, pypsaname * "_i") || ds.dim[pypsaname * "_i"] == 0
        return components
    end

    initialname = withname ? (name,) : ()

    df = DataFrame()
    if carrier === true
        df[!, :carrier] = _getattr(ds, pypsaname, :carrier)
    elseif carrier !== false
        if "buses_carrier" in ds
            df[!, :carrier] = getcarrier(ds, pypsaname, carrier)
        else
            initialname = (initialname..., join(repeat(["AC"], _length(carrier)), "_"))
        end
    end

    if exp !== false
        df[!, exp] = recode(_getattr(ds, pypsaname, exp),
                            0=>:fix, false=>:fix, 1=>:ext, true=>:ext)
    end
    if uc !== false
        df[!, uc] = recode(_getattr(ds, pypsaname, uc),
                           0=>:lin, false=>:lin, 1=>:uc, true=>:uc)
    end

    typesfn = joinpath(@__DIR__, string("pypsa.", pypsaname, ".types.csv"))
    types = isfile(typesfn) ? CSV.read(typesfn) : nothing

    componenttypeunion = resolve(Component, name)
    componentattributes = copy(attributes(componenttypeunion))
    componentattributes = componentattributes[componentattributes.quantitytype .!= "Variable", :]
    componentattributes[!, :PyPSA] = string.(componentattributes.attribute)
    componentattributes[!, :dimensions] = recode(length.(componentattributes.dimensions),
                                                 0=>:single, 1=>:static, 2=>:series,
                                                 3:20=>missing)

    idx = nomissing(ds[pypsaname * "_i"][:])
    timeattrs = componentattributes[componentattributes.dimensions .== :series, :attribute]
    for timeattr in timeattrs
        timeidx = string(pypsaname, "_t_", timeattr, "_i")
        if haskey(ds, timeidx)
            label = Symbol(:t_, timeattr)
            df[!, label] = recode(in.(idx, Ref(nomissing(ds[timeidx][:]))),
                                  false=>missing, true=>label)
        end
    end

    groups =
        if isempty(df)
            [(idx=1:size(idx, 1),
              name=Symbol(join(skipmissing(initialname), "_")),
              attrs=Dict{Symbol}{Any}())]
        else
            [(idx=_maybe_as_range(group.idx),
              name=Symbol(join(skipmissing(group.name), "_")),
              attrs=Dict(pairs(df[first(group.idx),:])...))
            for group in splitgroup(df, (idx=1:size(df, 1), name=initialname), names(df)...)]
        end

    for g in groups
        axis = Axis{g.name}(Symbol.(nomissing(ds[pypsaname * "_i"][:][g.idx])))

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

        attributes = attributedescriptions(g.name, ds, pypsaname, g.idx, axis; attributes=componentattributes)
        typeparams = typeparamdescriptions(g.name, ds, pypsaname, g.idx; types=types)

        push!(components, ComponentDesc(g.name, axis, componenttype, attributes, typeparams))
    end

    components
end


Base.get(::PypsaNcData, attrdesc::SingleAttrDesc, _) = attrdesc.value
Base.get(::PypsaNcData, attrdesc::MissingAttrDesc, ax) = AxisArray(fill(attrdesc.default, length(ax)), ax)

function Base.get(data::PypsaNcData, attrdesc::AttrDesc{T}, ax) where T
    da = data.dataset[attrdesc.pypsaname]
    dims = Symbol.(dimnames(da)[2:end])

    # Benchmarking shows that getting the full array first and then
    # subsetting is faster even if we need to get only 1 or 2 lines
    # We would need to improve NCDatasets first!
    AxisArray(astype.(T, da[:][attrdesc.indices,ntuple(i->:,length(dims))...]),
              ax, (axis(data, n) for n = dims)...)
end

function Base.get(data::PypsaNcData, component::Component, param::Symbol)
    componentdesc = data.components[naming(component)]
    get(data, componentdesc.attributes[param], componentdesc.axis)
end

EM.gettypeparams(data::PypsaNcData, device::Device) =
    data.components[naming(device)].typeparams

EM.axis(data::PypsaNcData, e::Component) = data.axes[naming(e)]
EM.axis(data::PypsaNcData, name::Symbol) = data.axes[name]

EM.components(data::PypsaNcData) = EM._components(data)

end

using .PyPSAData
