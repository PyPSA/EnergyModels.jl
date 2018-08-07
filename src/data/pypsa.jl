using DataFrames
using CSV
using Missings

abstract type AbstractPypsaAttrInfo end

struct PypsaAttrInfo{T} <: AbstractPypsaAttrInfo
    pypsaname::String
    indices::Union{Colon,UnitRange{Int64},StepRange{Int64,Int64},Vector{Int64}}
    default::Union{T,Missing}
end

struct SinglePypsaAttrInfo{T} <: AbstractPypsaAttrInfo
    value::T
end

struct MissingPypsaAttrInfo{T} <: AbstractPypsaAttrInfo
    default::T
end

struct PypsaClassInfo
    ctype::DataType
    class::Symbol
    names::Axis
    attrinfos::Dict{Symbol,AbstractPypsaAttrInfo}
    variables::Set{Symbol}
    typeparams::Dict{Symbol,Any}
end

struct PypsaNcData <: AbstractNcData
    dataset::Dataset
    components::Vector{DataType}
    classinfos::Dict{Symbol}{PypsaClassInfo}
end

function maybe_as_range(x::Vector{T}) where T<:Number
    if length(x) < 3 return x end
    d = diff(x)
    s = d[1]
    if all(d .== s)
        s == 1 ? x[1]:x[end] : x[1]:s:x[end]
    else
        x
    end
end

function splitbyattr(ds, listname, attr)
    if !haskey(ds, attr)
        return Dict(Symbol(listname)=>Colon())
    end

    da = ds[attr]
    df = DataFrame(indices=1:length(da), class=da[:])
    Dict(Symbol(g[1, :class]) => maybe_as_range(g[:indices])
         for g = groupby(df, :class))
end

PypsaAttrInfo(::Type{Val{:single}}, attr, name, ds, attrname, _, indices, names) =
    SinglePypsaAttrInfo(convert(Union{typenames[attr[:type]],Missing}, haskey(ds, attrname) ?
        @consense(ds[attrname][:][indices], "$name must be a single value") : _default(attr)))

PypsaAttrInfo(::Type{Val{:static}}, attr, name, ds, attrname, _, indices, names) =
    haskey(ds, attrname) ?
    PypsaAttrInfo{typenames[attr[:type]]}(attrname, indices, _default(attr)) : MissingPypsaAttrInfo(_default(attr))

function PypsaAttrInfo(::Type{Val{:series}}, attr, name, ds, attrname, attrname_t, indices, names)
    attrname_t_i = attrname_t * "_i"
    indices_t = findin(haskey(ds, attrname_t_i) ? ds[attrname_t_i][:] : [], names)
    if length(indices_t) == 0
        PypsaAttrInfo(Val{:static}, attr, name, ds, attrname, attrname_t, indices, names)
    else
        @assert(length(indices_t) == length(names),
                "$name has static and time-dependent $(attr[:PyPSA])")
        PypsaAttrInfo(attrname_t, indices_t, _default(attr))
    end
end

_default(attr) = typeparsers[attr[:type]](attr[:default])

PypsaAttrInfo(attr, name, ds, listname, indices, names) =
    PypsaAttrInfo(Val{Symbol(attr[:dimensions])}, attr, name, ds,
                  string(listname, "_", attr[:PyPSA]), string(listname, "_t_", attr[:PyPSA]),
                  indices, names)

pypsaattrinfos(attrs, args...) = Dict(attr[:EM]=>PypsaAttrInfo(attr, args...)
                                      for attr = eachrow(attrs))

function pypsavariables(attrs, name, ds, listname, indices)
    variables = Set{Symbol}()
    if haskey(attrs, :variable_switch)
        for attr = eachrow(attrs[.!ismissing.(attrs[:variable_switch]), :])
            switchname = string(listname, "_", attr[:variable_switch])
            if haskey(ds, switchname) &&
               @consense(ds[switchname][:][indices],
                         "$(attr[:variable_switch]) of $name must either be true or false") == 1
                push!(variables, attr[:EM])
            end
        end
    end
    variables
end

function pypsatypeparams(name, ds, listname, indices, types)
    if types === nothing return Dict() end
    typename = @consense(ds[string(listname, "_type")][:][indices], "$name may not have more than one type")
    typ = types[types[:name] .== typename, :]
    Dict(c => typ[1, c] for c = names(typ) if c != :name)
end

function pypsaclassinfos(ds, listname, ctype)
    classinfos = Dict{Symbol, PypsaClassInfo}()

    attrs = CSV.read(joinpath(@__DIR__, string("pypsa.", listname, ".csv"));
                     truestring="t", falsestring="f")
    attrs[:EM] = Symbol.(attrs[:EM])

    typesfn = joinpath(@__DIR__, string("pypsa.", listname, ".types.csv"))
    types = isfile(typesfn) ? CSV.read(typesfn) : nothing

    for (class, indices) = splitbyattr(ds, listname, listname * "_carrier")
        names = disallowmissing(ds[listname * "_i"][:][indices])
        name = string(listname, "::", class)
        attrinfos = pypsaattrinfos(attrs, name, ds, listname, indices, names)
        variables = pypsavariables(attrs, name, ds, listname, indices)
        typeparams = pypsatypeparams(name, ds, listname, indices, types)
        classinfos[class] = PypsaClassInfo(ctype, class, Axis{Symbol(naming(ctype))}(names), attrinfos, variables, typeparams)
    end

    classinfos
end

"""
    PypsaNcData

"""
function Base.show(io::IO, data::PypsaNcData)
    println(io, string(typeof(data)), " based on '", path(data.dataset), "' describes")
    for c = data.components[1:end]
        class = (string(class, " (", length(data.classinfos[class].names), ")") for class = classes(data, c))
        println(io, "    ", naming(c), ": ", join(class, ", "))
    end
end

function PypsaNcData(ds)
    # Check for available components by looking for size of _i coordinate
    # For one component something like
    pypsacomponents = CSV.read(joinpath(@__DIR__, "pypsa.components.csv"))

    # Determine defined components and split them into classes
    # Work In Progress: for now we assume that splitting on carrier is good enough
    components = DataType[]
    classinfos = Dict{Symbol}{PypsaClassInfo}()
    for row = eachrow(pypsacomponents)
        listname = row[:listname]

        # Resolve julia types in the EnergyModels module (i'm not sure if that's
        # the most sensible way to do it)
        ctype = resolve(Symbol(row[:componenttype]))

        if !haskey(ds.dim, listname * "_i") || ds.dim[listname * "_i"] == 0
            continue
        end

        push!(components, ctype)
        merge!(classinfos, pypsaclassinfos(ds, listname, ctype))
    end

    # ## Below there is a try to figure out the splits based on grouping all
    # ## information that could change

    # # Classes should split on carrier and variants as defined (fex for generators) by:
    # # - generators_i
    # # - generators_p_nom_extendable
    # # - generators_t_p_max_pu_i
    # # - generators_t_p_min_pu_i
    # # - generators_committable

    # jtype = Generator
    # c = pypsacomponents[jtype]

    # c = @NT(jtype=EM.Generator,
    #         listname="generators",
    #         variantattrs=[:p_nom_extendable, :committable],
    #         timedepattrs=[:p_max_pu, :p_min_pu, :marginal_cost])

    # # Build feature dataframe
    # df = DataFrame(name=as_data(ds[string(c.listname, "_i")]))
    # df[:inds] = 1:length(df)
    # for attr = c.variantattrs
    #     key = string(c.listname, "_", attr)
    #     if haskey(ds, key) df[attr] = as_data(ds[key], Vector{Bool}) end
    # end
    # for attr = c.timedepattrs
    #     key = string(c.listname, "_t_", attr, "_i")
    #     if haskey(ds, key)
    #         df[attr] = false
    #         df[attr][findin(df[:name], as_data(ds[key]))] = true
    #     end
    # end
    # map(groupby(df, df.colindex.colnames[2:end])) do x
    #     @NT(variant = squeeze(convert(Array{Bool}{2}, df[1,2:end]), 1),
    #         index = df[:inds])
    # end

    PypsaNcData(ds, components, classinfos)
end


Base.get(::PypsaNcData, attrinfo::SinglePypsaAttrInfo, _) = attrinfo.value
Base.get(::PypsaNcData, attrinfo::MissingPypsaAttrInfo, ax) = AxisArray(fill(attrinfo.default, length(ax)), ax)

function Base.get(data::PypsaNcData, attrinfo::PypsaAttrInfo{T}, ax) where T
    da = data.dataset[attrinfo.pypsaname]
    dims = dimnames(da)[2:end]

    as_dtype(T, a) = convert(Array{T,ndims(a)}, a)

    # Benchmarking shows that getting the full array first and then
    # subsetting is faster even if we need to get only 1 or 2 lines
    # We would need to improve NCDatasets first!
    AxisArray(as_dtype(T, da[:][attrinfo.indices,ntuple(i->:,length(dims))...]),
              ax, (axis(data, n) for n = dims)...)
end

function Base.get(data::PypsaNcData, component::Component, class::Symbol, param::Symbol)
    classinfo = data.classinfos[class]
    get(data, classinfo.attrinfos[param], classinfo.names)
end

gettypeparams(data::PypsaNcData, component::Component, class::Symbol) =
    data.classinfos[class].typeparams

isvar(data::PypsaNcData, component::Component, class::Symbol, param::Symbol) =
    in(param, data.classinfos[class].variables)

axis(data::PypsaNcData, c::Component, class) = data.classinfos[class].names

components(data::PypsaNcData) = data.components
classes(data::PypsaNcData, T) = (cl.class for cl = values(data.classinfos) if cl.ctype === T)
