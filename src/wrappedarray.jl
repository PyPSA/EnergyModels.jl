using Base: @propagate_inbounds, HasShape, HasEltype
import AxisArrays: axisdim, axisnames, axisname, axisvalues
import JuMP: JuMPArray

struct WrappedArray{T,N,M,D,Ax} <: AbstractArray{T,N}
    data::D
    axes::Ax
    # Duck-typing, we assume axisnames is defined for data
    function WrappedArray(data::Union{AxisArray{T,M},JuMP.JuMPArray{T,M}}, axes::NTuple{N,Axis}) where {T,N,M}
        @assert(issubset(axisnames(data), axisnames(axes...)),
                "WrappedArray is missing axes: $(setdiff(axisnames(data), axisnames(axes...)))")
        new{T,N,M,typeof(data),typeof(axes)}(data, axes)
    end
end
WrappedArray(data, axes::Axis...) = WrappedArray(data, axes)
# Not all axes are Axis, since we insist, you have to provide a function that
# convert one for your iterator type by adding methods to this recursive
# _to_axis
WrappedArray(data, axes...) = WrappedArray(data, _to_axis(axes...)...)
_to_axis(ax::Axis, axes...) = (ax, _to_axis(axes...)...)
_to_axis() = ()

@inline Base.size(A::WrappedArray) = length.(A.axes)
@inline Base.size(A::WrappedArray, Ax::Axis) = length(A.axes[axisdim(A, Ax)])

# Slightly adapted axisdim, axisnames and axisvalues from AxisArrays

axisdim(A::WrappedArray, ax::Axis) = axisdim(A, typeof(ax))
@generated function axisdim(A::WrappedArray, ax::Type{Ax}) where Ax<:Axis
    dim = axisdim(A, Ax)
    :($dim)
end

# The actual computation is done in the type domain, which is a little tricky
# due to type invariance.
function axisdim(::Type{WrappedArray{T,N,M,D,Ax}}, ::Type{<:Axis{name,S} where S}) where {T,N,M,D,Ax,name}
    isa(name, Int) && return name <= N ? name : error("axis $name greater than array dimensionality $N")
    names = axisnames(Ax)
    idx = findfirst(names, name)
    idx == 0 && error("axis $name not found in array axes $names")
    idx
end

axisnames(::WrappedArray{T,N,M,D,Ax}) where {T,N,M,D,Ax}       = AxisArrays._axisnames(Ax)
axisnames(::Type{WrappedArray{T,N,M,D,Ax}}) where {T,N,M,D,Ax} = AxisArrays._axisnames(Ax)
axisvalues(A::WrappedArray) = axisvalues(A.axes...)

# Probably if there are colons or ranges on any of the virtual axes,
# we would instead want to give back a new WrappedArray, but this works well enough.
@generated function Base.getindex(A::WrappedArray{T,N,M,D,Ax}, idxs...) where {T,N,M,D,Ax}
    meta = Expr(:meta, :inline, :propagate_inbounds)
    names = axisnames(A)
    :($meta; A.data[$((Expr(:ref, :idxs, findfirst(names, n)) for n=axisnames(D))...)])
end

# iteration on Axis
@inline Base.start(A::Axis) = 1
@propagate_inbounds Base.next(A::Axis, i) = (A[i], i+1)
@propagate_inbounds Base.done(A::Axis, i) = length(A) + 1 == i

# iteration traits
Base.iteratorsize(::Type{<:Axis}) = HasShape()
Base.iteratoreltype(::Type{<:Axis}) = HasEltype()
