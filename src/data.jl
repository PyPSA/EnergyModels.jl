using NCDatasets

using DataFrames
using CSV

struct NcData <: Data
    dataset::Dataset
end

struct PypsaNcData <: Data
    dataset::Dataset
end

# struct QAxis
#     name::Symbol
#     length::Int32
# end

# Later we should represent variants more by something like this
#
# struct Quantity
#     name::Symbol
#     isvariable::Bool
#     dimensions::Tuple{Vararg{Symbol}}
#     Quantity(name, isvariable, dimensions) = new(name, isvariable, tuple(sort!(collect(dimensions))...))
# end
#
# const Variant = NTuple{N,Quantity} where N
# Variant(quantities::Vararg{Quantity}) = Variant(sort!(collect(quantities)))
# Base.isless(a::Quantity, b::Quantity) = a.isvariable != b.isvariable ? a.isvariable < b.isvariable : a.name < b.name
# Base.show(io::IO, v::Variant) = for q=v print(io, "::"); show(io, q) end
# Base.show(io::IO, q::Quantity) = print(io, string(q.name, q.isvariable ? "_v" : "", "_", join(string.(q.dimensions), ":")))

function splitbyattr(ds, attr)
    if !in(attr, ds)
        return Dict(:general=>Colon())
    end

    da = ds[attr]
    df = DataFrame(inds=1:length(da), class=da[:])
    classes = Dict{Symbol, eltype(df[:inds])}()
    for g = groupby(df, :class)
        classes[g[1, :class]] = g[:inds]
    end
    classes
end

"""
    PypsaNc

"""
function PypsaNcData(ds)
    # Needs to know about available components and their mapping
    # generators -> Generator
    # storage_units -> StorageUnit
    # loads -> Load
    # buses -> Bus (is a component?)
    # lines -> Line
    # links -> Link
    # transformers -> Transformer
    # SubNetwork is always there

    # Check for available components by looking for size of _i coordinate
    # For one component something like
    pypsacomponents = CSV.read(joinpath(Pkg.dir("EnergyModels"), "src", "pypsa.components.csv"))

    # Resolve julia types in the EnergyModels module
    pypsacomponents[:componenttype] = getfield.(current_module(), Symbol.(pypsacomponents[:componenttype]))

    # componenttypes = [c[:componenttype]
    #                   for c = eachrow(pypsacomponents)
    #                   if haskey(ds.dim, c[:listname]) && ds.dim[c[:listname]] > 0]

    # Determine defined components and split them into classes
    # Work In Progress: for now we assume that splitting on carrier is good enough
    components = Component[]
    splits_by_component = Dict{String}{Dict{Symbol}{Any}}()
    mapping = Dict{String}{Dict{Symbol}{String}}()
    for row = eachrow(pypsacomponents)
        listname = row[:listname]
        ctype = row[:componenttype]

        if !haskey(ds.dim, listname) || ds.dim[listname] == 0
            continue
        end

        splits = splitbyattr(ds, listname * "_carrier")
        splits_by_component[naming(ctype)] = splits

        # TODO read in mapping between pypsa component names and EnergyModels
        CSV.read(joinpath(Pkg.dir("EnergyModels"), "src", string("pypsa.", listname, ".csv")))
        

        append!(components, ctype.(keys(splits)))
    end





    ## Below there is a try to figure out the splits based on grouping all
    ## information that could change

    # Classes should split on carrier and variants as defined (fex for generators) by:
    # - generators_i
    # - generators_p_nom_extendable
    # - generators_t_p_max_pu_i
    # - generators_t_p_min_pu_i
    # - generators_committable

    jtype = Generator
    c = pypsacomponents[jtype]

    c = @NT(jtype=EM.Generator,
            listname="generators",
            variantattrs=[:p_nom_extendable, :committable],
            timedepattrs=[:p_max_pu, :p_min_pu, :marginal_cost])

    # Build feature dataframe
    df = DataFrame(name=as_data(ds[string(c.listname, "_i")]))
    df[:inds] = 1:length(df)
    for attr = c.variantattrs
        key = string(c.listname, "_", attr)
        if haskey(ds, key) df[attr] = as_data(ds[key], Vector{Bool}) end
    end
    for attr = c.timedepattrs
        key = string(c.listname, "_t_", attr, "_i")
        if haskey(ds, key)
            df[attr] = false
            df[attr][findin(df[:name], as_data(ds[key]))] = true
        end
    end
    map(groupby(df, df.colindex.colnames[2:end])) do x
        @NT(variant = squeeze(convert(Array{Bool}{2}, df[1,2:end]), 1),
            index = df[:inds])
    end
end

function NcData(filename::String)
    ds = Dataset(filename)
    (haskey(ds.attrs, "network_pypsa_version") ? PypsaNcData : NcData)(ds)
end

function load(filename::String)::Data
    if endswith(filename, ".nc")
        NcData(filename)
    else
        error("file ending of '$filename' not recognized")
    end
end

"""
Read dataarray from netcdf file as AxisArray
"""

as_data(x) = nomissing(x[:])
as_data(x, ::Type{T}) where T = convert(T, x)

function as_axisarray(ds::Dataset, name::String)
    da = ds[name]
    AxisArray(as_data(da), (Axis{Symbol(n)}(as_data(ds[n]))
                            for n=dimnames(da))...)
end

naming(component::T) where T<:Component = lowercase(string(T, "s"))

function naming(component::Component, class::Symbol, param::Symbol)
    join(string.((naming(component), class, param)), "::")
end

Base.get(data::NcData, args...) = as_axisarray(data.dataset, naming(args...))
