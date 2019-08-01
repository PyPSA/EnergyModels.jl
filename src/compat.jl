import AxisArrays: AxisArray, axisdim, axisnames, axisvalues, CategoricalVector

tofloat(x::String) = parse(Float64, x)
tofloat(x) = float(x)

const typeparsers = Dict("float"=>tofloat, "string"=>Symbol,
                         "int"=>x->parse(Int, x),
                         "bool"=>let d=Dict('t'=>true, 'f'=>false); x->d[first(x)] end)
const typenames = Dict("float"=>Float64, "string"=>Symbol, "int"=>Int64, "bool"=>Bool)
const Scalar = Union{Float64,Symbol,Int64,Bool}

astype(typ::String, x::String) = typeparsers[typ](x)
astype(typ::String, x) = astype(typenames[typ], x)
astype(::String, ::Missing) = missing
astype(::DataType, ::Missing) = missing
astype(::Type{Symbol}, x) = Symbol(x)
astype(::Type{T}, x) where T = convert(Union{T,Missing}, x)

# !!Type-piracy!! We should make the case to move these definitions to JuMP
# Will be difficult as long as they use their one JuMPArray methods!
axisnames(::JuMP.Containers.DenseAxisArray{T,N,Ax}) where {T,N,Ax} = AxisArrays._axisnames(Ax)
axisnames(::Type{JuMP.Containers.DenseAxisArray{T,N,Ax}}) where {T,N,Ax} = AxisArrays._axisnames(Ax)
axisvalues(A::JuMP.Containers.DenseAxisArray) = axisvalues(A.axes...)

function axisdim(::Type{JuMP.Containers.DenseAxisArray{T,N,Ax,D}}, ::Type{<:Axis{name}}) where {T,N,Ax,name,D}
    isa(name, Int) && return name <= N ? name : error("axis $name greater than array dimensionality $N")
    names = axisnames(Ax)
    idx = findfirst(isequal(name), names)
    isnothing(idx) && error("axis $name not found in array axes $names")
    idx
end

axisdim(A::JuMP.Containers.DenseAxisArray, ax::Axis) = axisdim(A, typeof(ax))
@generated function axisdim(A::JuMP.Containers.DenseAxisArray, ax::Type{Ax}) where Ax<:Axis
    dim = axisdim(A, Ax)
    :($dim)
end

# AxisArray(A::JuMPArray) = AxisArray(A.innerArray, A.indexsets...)
AxisArray(A::JuMP.Containers.DenseAxisArray) = AxisArray(A.data, A.axes...)

function _shiftamt(A, shifts::Pair{Symbol,T}...) where T<:Integer
    amt = zeros(T, ndims(A))
    for (ax, s) = shifts
        amt[axisdim(A, Axis{ax})] += s
    end
    amt
end

Base.circshift(A::AxisArray, shifts::Pair{Symbol,<:Integer}...) = circshift(A, _shiftamt(A, shifts...))
function Base.circshift(A::Containers.DenseAxisArray, shifts::Pair{Symbol,<:Integer}...)
    JuMP.Containers.DenseAxisArray(circshift(A.data, _shiftamt(A, shifts...)), A.axes...)
end

Base.indexin(a::AbstractArray, b::Axis) = indexin(a, b.val)
Base.findall(pred::Base.Fix2{typeof(in), <:AbstractArray}, b::Axis) = findall(pred, b.val)
Base.findfirst(pred::Base.Fix2{typeof(isequal)}, b::Axis) = findfirst(pred, b.val)

# vcat definition for categorical vector
Base.vcat(As::CategoricalVector...) = CategoricalVector(vcat(map(A -> A.data, As)...))

# function JuMP.constructvariable!(m::Model, _error::Function, lowerbound::Number, upperbound::AffExpr, args...; kwargs...)
#     v = constructvariable!(m, _error, lowerbound, Inf, args...; kwargs...)
#     addconstraint(m, constructconstraint!(addtoexpr(upperbound, -1.0, v), :(>=)))
#     v
# end

# function JuMP.constructvariable!(m::Model, _error::Function, lowerbound::AffExpr, upperbound::Number, args...; kwargs...)
#     v = constructvariable!(m, _error, -Inf, upperbound, args...; kwargs...)
#     addconstraint(m, constructconstraint!(addtoexpr(lowerbound, -1.0, v), :(<=)))
#     v
# end

# function JuMP.constructvariable!(m::Model, _error::Function, lowerbound::AffExpr, upperbound::AffExpr, args...; kwargs...)
#     v = constructvariable!(m, _error, -Inf, Inf, args...; kwargs...)
#     addconstraint(m, constructconstraint!(addtoexpr(lowerbound, -1.0, v), :(<=)))
#     addconstraint(m, constructconstraint!(addtoexpr(upperbound, -1.0, v), :(>=)))
#     v
# end
