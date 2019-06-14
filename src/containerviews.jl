abstract type AbstractContainerView{T} end

struct ContainerView{T} <: AbstractContainerView{T}
    model::EnergyModel
    devices::Dict{Symbol,T}
end

struct SubContainerView{T} <: AbstractContainerView{T}
    model::EnergyModel
    devices::Dict{Symbol,T}
    buses::Axis
end

tupleaxisvals(c::Bus, A) = A.val
tupleaxisvals(c) = tupleaxisvals(c, axis(c))
tupleaxisvals(c, A) = AxisArrays.CategoricalVector(tuple.(c.class, A.val))

_maybesubset(cv::ContainerView, c, A) = A
_maybesubset(cv::SubContainerView, c::Component, A) = A[findall(in(cv.buses), c), ntuple(i->:,ndims(A)-1)...]

_viewaxis(cv::AbstractContainerView{T}, a) where T = Axis{Symbol(naming(T))}(a)

devices(cv::AbstractContainerView) = values(cv.devices)
devices(cv::AbstractContainerView, T::Type{<:Device}) = (c for c = devices(cv) if isa(c, T))

axis(cv::AbstractContainerView) = _viewaxis(cv, vcat((_maybesubset(cv, c, tupleaxisvals(c)) for c = devices(cv))...))
axis(cv::AbstractContainerView, T::Type{<:Component}) = axis(m[T])

Base.cat(cv::AbstractContainerView, cs::Vector{<:Component}, as::Vector) = @consense(as, "Single values do not agree")
function Base.cat(cv::AbstractContainerView, cs::Vector{<:Component}, as::Vector{<:AxisArray})
    _viewaxisarray(c, a) = AxisArray(a, _viewaxis(cv, tupleaxisvals(c, first(AxisArrays.axes(a)))))
    cat((_viewaxisarray(c, _maybesubset(cv, c, a)) for (c, a) = zip(cs, as))...; dims=1)
end

mapcat(f::Function, cv::AbstractContainerView) = cat(cv, collect(devices(cv)), map(f, devices(cv)))

# TODO The AxisArray that is spliced in here is a mild hack!
Base.get(cv::AbstractContainerView, i) = mapcat(c->AxisArray(get(c, i)), cv)
Base.get(cv::AbstractContainerView, i, axes...) = WrappedArray(get(cv, i), axes...)

Base.getindex(cv::AbstractContainerView, i) = get(cv, i)
