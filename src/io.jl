using NCDatasets

abstract type IO end

struct NcIO <: IO
    dataset::Dataset
    filename::String
end

NcIO(filename::String) = NcIO(Dataset(filename), filename)

function load(filename::String) -> IO
    if endswith(filename, ".nc")
        NcIO(filename)
    else
        error("file ending of '$filename' not recognized")
    end
end

"""
Read dataarray from netcdf file as AxisArray
"""
function as_axisarray(ds::Dataset, name::String)
    da = io.dataset[name]
    as_data(x) = nomissing(x[:])
    AxisArray(as_data(da), (Axis{Symbol(n)}(as_data(io.dataset[n]))
                            for n=dimnames(da))...)
end

function naming(component::T, class::Symbol, param:Symbol) where T<:Component
    join(String.((lowercase(String(Symbol(T))) * "s", class, param)), "::")
end

get(io::NcIO, args...) = as_axisarray(io.dataset, naming(args...))
