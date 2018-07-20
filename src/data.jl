using NCDatasets

struct NcData <: Data
    dataset::Dataset
    filename::String
end

struct PypsaNcData <: Data
    dataset::Dataset
    available::
end

function PypsaNcData(ds, filename)
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
    jtypes = [c.jtype
              for c = pypsacomponents
              if haskey(ds.dim, c.listname) && ds.dim[c.listname] > 0]

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

    components = CSV.read(joinpath(@__DIR__, 'pypsa.generators.csv'))
    Dict()
    for c = classes, attr = attrs
        inds = findin(ds[string(listname, "_t_", attr, "_i")], ds[string(listname, "_i")][c.inds])
        if length(inds) == 0 continue end
        @assert length(inds) == length(c.inds)

    end
end

function NcData(filename::String)
    ds = Dataset(filename)
    (haskey(ds.attrs, "network_pypsa_version") ? PypsaNcData : NcData)(ds, filename)
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

function naming(component::T, class::Symbol, param::Symbol) where T<:Component
    join(string.((lowercase(string(T, "s")), class, param)), "::")
end

Base.get(data::NcData, args...) = as_axisarray(data.dataset, naming(args...))
