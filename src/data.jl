using NCDatasets

abstract type AbstractNcData <: Data end

include("data/pypsa.jl")

struct NcData <: Data
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

# or even type based as a variant of

# struct QAxis{name,N} end

# struct Quantity{name,isvar,N,Ax}
#     Quantity{name,isvar,N,Ax}() where {name,isvar<:Bool,N,Ax<:Tuple{Vararg{QAxis,N}}} = new{name,isvar,N,Ax}()
# end

function NcData(filename::String)
    ds = Dataset(filename)
    (haskey(ds.attrib, "network_pypsa_version") ? PypsaNcData : NcData)(ds)
end

function load(filename::String)::Data
    if endswith(filename, ".nc")
        NcData(filename)
    else
        error("file ending of '$filename' not recognized")
    end
end

resolve(ctype::Symbol) = getfield(EnergyModels, ctype)

naming(::Type{Symbol}, c::Component, symbols...) = Symbol(naming(c, symbols...))
naming(::T, symbols...) where T<:Component = join((naming(T), string.(symbols)...), "::")
function naming(::Type{T}) where T<:Component
    s = lowercase(string(T))
    s[findlast(s, '.')+1:end] * (s[end] == 's' ? "es" : "s")
end

function Base.get(data::NcData, args...)
    da = data.dataset[naming(args...)]
    AxisArray(da[:], (axis(data, n) for n = dimnames(da))...)
end

axis(data::AbstractNcData, n::Symbol) = axis(data, String(n))
axis(data::AbstractNcData, n::String) = Axis{Symbol(n)}(disallowmissing(data.dataset[n][:]))

# Interface for the abstract type Data
#
# * Required methods
#
# Base.get(data, component, class, param)
# returns an AxisArray of `param` from `component::class`
#
# components(data)
# returns Array{DataType} with the described components
#
# classes(data, ::Type{T}) where T<:Component
# returns Array{Symbol} of the described classes
