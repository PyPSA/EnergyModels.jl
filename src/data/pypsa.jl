module PyPSAData

using AxisArrays
using NCDatasets
using DataFrames
using CSV

using ..EnergyModels
const EM = EnergyModels

using ..EnergyModels:
    AbstractNcData, resolve, @consense, Component, Device, DeviceFormulation,
    attributes, components, typenames, naming

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

"""
    _dimnames(::PypsaNcData, ::AbstractAttrDesc)

returns tuples for the dimensions EXCEPT for the main component axis.
"""
_dimnames(attrdesc::AbstractAttrDesc; ds=nothing) = ()
_dimnames(attrdesc::AttrDesc; ds=nothing) = dimnames(ds[attrdesc.pypsaname])[2:end]

function AxisArrays.axes(cd::ComponentDesc; ds=nothing)
    dims = unique(Iterators.flatten((_dimnames(attrdesc; ds=ds) for attrdesc in values(cd.attributes))))
    (cd.axis, (Axis{Symbol(n)}(nomissing(ds[n][:])) for n = dims)...)
end

struct PypsaNcData <: AbstractNcData
    dataset::Dataset
    components::Dict{Symbol}{ComponentDesc}
    axes::Dict{Symbol}{Axis}
end

_getattr(ds, pypsaname, attr) =
    haskey(ds, attr) ? nomissing(ds[attr][:]) : Array{Union{Float64,Missing}}(missing, length(ds[string(pypsaname, "_i")]))

_getattrpair(ds, pypsaname, attr) = attr => _getattr(ds, pypsaname, string(pypsaname, "_", attr))
_getattrpair(ds, pypsaname, attr::Pair) = _getattrpair(ds, pypsaname, attr.first)
_getattrpair(ds, pypsaname, attr::DataFrame) = ((col,)=names(df); col=>attr[col])

splitgroup(df, group) = [group]
splitgroup(df, group, pair::Pair, attrs...) = splitgroup(df, group, pair.first, attrs...; usename=x->pair.second[x ? 1 : 2])
function splitgroup(df, group, attr, attrs...; usename=identity)
    gdf = groupby(DataFrame(attr=>df[attr][group.idx], :idx=>group.idx, copycols=false), attr);

    if length(gdf) == 1
        ng = attr == :carrier ? (idx=group.idx, name=(group.name..., usename(first(parent(gdf)[attr])))) : group
        groups = splitgroup(df, ng, attrs...)
    else
        groups = Any[]
        for sdf in gdf
            ng = (idx=collect(sdf.idx),
                  name=(group.name..., usename(first(sdf[attr]))))
            append!(groups, splitgroup(df, ng, attrs...))
        end
    end

    groups
end

function splitintogroups(ds, pypsaname, attrs...; initialname=())
    df = DataFrame((_getattrpair(ds, pypsaname, attr) for attr in attrs)...,
                   copycols=false)
    if isempty(df)
        [(idx=1:size(ds[string(pypsaname, "_i")], 1),
          name=Symbol(join(skipmissing(initialname), "_")),
          attrs=Dict{Symbol}{Any}())]
    else
        [(idx=_maybe_as_range(group.idx),
          name=Symbol(join(skipmissing(group.name), "_")),
          attrs=Dict(pairs(df[first(group.idx),:])...))
         for group in splitgroup(df, (idx=1:size(df, 1), name=initialname), attrs...)]
    end
end

AttrDesc(::Type{Val{:single}}, attr, name, ds, attrname, attrname_t, idx, axis) =
    SingleAttrDesc(convert(Union{typenames[attr[:dtype]],Missing}, haskey(ds, attrname) ?
        @consense(ds[attrname][:][idx], "$name must be a single value") : attr[:default]))

AttrDesc(::Type{Val{:static}}, attr, name, ds, attrname, attrname_t, idx, axis) =
    haskey(ds, attrname) ?
    AttrDesc{typenames[attr[:dtype]]}(attrname, idx, attr[:default]) :
    MissingAttrDesc(attr[:default])

function AttrDesc(::Type{Val{:series}}, attr, name, ds, attrname, attrname_t, idx, axis)
    attrname_t_i = attrname_t * "_i"
    indices_t = haskey(ds, attrname_t_i) ? findall(in(ds[attrname_t_i][:]), axis) : []
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
    typ = types[types[:name] .== typename, :]
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
    axdict = EM.collectaxes(components; ds=ds)
    PypsaNcData(ds, compdict, axdict)
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
    DataFrame(carrier=getcarrier(ds, buses_carrier, pypsaname, busattrs))
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

    attrs = []
    if carrier === true
        push!(attrs, :carrier)
    elseif carrier !== false
        if "buses_carrier" in ds
            push!(attrs, getcarrier(ds, carrier))
        else
            initialname = (initialname..., join(repeat(["AC"], _length(carrier)), "_"))
        end
    end

    if exp !== false push!(attrs, exp => (:ext, :fix)) end
    if uc !== false push!(attrs, uc => (:uc, :lin)) end

    typesfn = joinpath(@__DIR__, string("pypsa.", pypsaname, ".types.csv"))
    types = isfile(typesfn) ? CSV.read(typesfn) : nothing

    componenttypeunion = resolve(Component, name)
    componentattributes = copy(attributes(componenttypeunion))
    componentattributes = componentattributes[componentattributes[:quantitytype] .!= "Variable", :]
    componentattributes[:PyPSA] = string.(componentattributes[:attribute])
    componentattributes[:dimensions] = recode(length.(componentattributes[:dimensions]),
                                              0=>:single, 1=>:static, 2=>:series,
                                              3:20=>missing)

    for g in splitintogroups(ds, pypsaname, attrs...; initialname=initialname)
        axis = Axis{g.name}(nomissing(ds[pypsaname * "_i"][:][g.idx]))

        formulation = join(skipmissing([
            exp !== false ? (Bool(coalesce(g.attrs[exp], false)) ? "linexp" : missing) : missing,
            uc !== false ?  (Bool(coalesce(g.attrs[uc], false))  ? "ucdisp" : "lindisp") : "lindisp"
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

    as_dtype(T, a) = convert(Array{T,ndims(a)}, a)

    # Benchmarking shows that getting the full array first and then
    # subsetting is faster even if we need to get only 1 or 2 lines
    # We would need to improve NCDatasets first!
    AxisArray(as_dtype(T, da[:][attrdesc.indices,ntuple(i->:,length(dims))...]),
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
