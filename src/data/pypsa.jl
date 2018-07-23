using DataFrames
using CSV
using Missings

struct PypsaAttrInfo
    pypsaname::String
    indices::Union{Colon,UnitRange{Int64},StepRange{Int64,Int64},Vector{Int64}}
    default::Union{Missing,Float64,Bool,String,Int64}
end

struct PypsaClassInfo
    class::Symbol
    names::Vector{String}
    attrinfos::Dict{Symbol,PypsaAttrInfo}
    variables::Set{Symbol}
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

function splitbyattr(ds, attr)
    if !haskey(ds, attr)
        return Dict(:undef=>Colon())
    end

    da = ds[attr]
    df = DataFrame(indices=1:length(da), class=da[:])
    Dict(Symbol(g[1, :class]) => maybe_as_range(g[:indices])
         for g = groupby(df, :class))
end

function pypsaclassinfos(ds, listname)
    classinfos = Dict{Symbol, PypsaClassInfo}()

    attrs = CSV.read(joinpath(@__DIR__, string("pypsa.", listname, ".csv"));
                     truestring="t", falsestring="f")
    attrs[:time_dependent] = recode(attrs[:time_dependent], missing=>false)
    attrs[:EM] = Symbol.(attrs[:EM])

    for (class, indices) = splitbyattr(ds, listname * "_carrier")
        names = disallowmissing(ds[listname * "_i"][indices])
        attrinfos = Dict{Symbol}{PypsaAttrInfo}()
        variables = Set{Symbol}()

        for attr = eachrow(attrs)
            attrname_t = string(listname, "_t_", attr[:PyPSA])
            if attr[:time_dependent] && haskey(ds, attrname_t * "_i")
                indices_t = findin(ds[attrname_t * "_i"], names)
            else
                indices_t = Int64[]
            end

            attrinfos[attr[:EM]] =
                if length(indices_t) > 0
                    @assert(length(indices_t) == length(names),
                            "$listname::$class has static and time-dependent $(attr[:PyPSA])")
                    PypsaAttrInfo(attrname_t, indices_t, attr[:default])
                else
                    PypsaAttrInfo(string(listname, "_", attr[:PyPSA]), indices, attr[:default])
                end
        end

        if haskey(attrs, :variable_switch)
            for attr = eachrow(@views attrs[.!ismissing.(attrs[:variable_switch]), :])
                switchname = string(listname, "_", attr[:variable_switch])
                if !haskey(ds, switchname) continue end
                isvariable = ds[switchname][indices]
                if any(isvariable)
                    @assert all(isvariable) "$(attr[:variable_switch]) of $listname::$class must either be true or false (not both)"
                    push!(variables, attr[:EM])
                end
            end
        end

        classinfos[class] = PypsaClassInfo(class, names, attrinfos, variables)
    end

    classinfos
end

"""
    PypsaNcData

"""
struct PypsaNcData <: AbstractNcData
    dataset::Dataset
    components::Vector{DataType}
    classinfos::Dict{String}{Dict{Symbol}{PypsaClassInfo}}
end

function Base.show(io::IO, data::PypsaNcData)
    println(io, string(typeof(data)), " based on '", path(data.dataset), "' describes")
    for c = data.components[1:end]
        ci = data.classinfos[naming(c)]
        classes = (string(cl.class, " (", length(cl.names), ")") for cl = values(ci))
        println(io, "    ", naming(c), ": ", join(classes, ", "))
    end
end

function PypsaNcData(ds)
    # Check for available components by looking for size of _i coordinate
    # For one component something like
    pypsacomponents = CSV.read(joinpath(@__DIR__, "pypsa.components.csv"))

    # Determine defined components and split them into classes
    # Work In Progress: for now we assume that splitting on carrier is good enough
    components = DataType[]
    classinfos = Dict{String}{Dict{Symbol}{PypsaClassInfo}}()
    for row = eachrow(pypsacomponents)
        listname = row[:listname]

        # Resolve julia types in the EnergyModels module (i'm not sure if that's
        # the most sensible way to do it)
        ctype = getfield(EnergyModels, Symbol.(row[:componenttype]))

        if !haskey(ds.dim, listname * "_i") || ds.dim[listname * "_i"] == 0
            continue
        end

        push!(components, ctype)
        classinfos[naming(ctype)] = pypsaclassinfos(ds, listname)
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

function Base.get(data::PypsaNcData, component::Component, class::Symbol, param::Symbol)
    name = naming(component)
    classinfo = data.classinfos[name][class]
    attrinfo = classinfo.attrinfos[param]

    if !haskey(data.dataset, attrinfo.pypsaname)
        AxisArray(fill(attrinfo.default, length(classinfo.names)),
                  Axis{Symbol(name)}(classinfo.names))
    else
        da = data.dataset[attrinfo.pypsaname]
        dims = dimnames(da)[2:end]

        # Benchmarking shows that getting the full array first and then
        # subsetting is faster even if we need to get only 1 or 2 lines
        # We would need to improve NCDatasets first!
        AxisArray(da[:][attrinfo.indices,ntuple(i->:,length(dims))...],
                  Axis{Symbol(name)}(classinfo.names),
                  (axis(data, n) for n = dims)...)
    end
end

isvar(data::PypsaNcData, component::Component, class::Symbol, param::Symbol) =
    in(param, data.classinfos[naming(component)][class].variables)

function axis(data::PypsaNcData, c::Component, class)
    name = naming(c)
    Axis{Symbol(name)}(data.classinfos[name][class].names)
end

components(data::PypsaNcData) = data.components
classes(data::PypsaNcData, T) = keys(data.classinfos[naming(T)])
