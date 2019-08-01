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

_wrapaxis(cv::AbstractContainerView{T}, a) where T = Axis{Symbol(naming(T))}(a)
function _wrapaxisarray(cv::AbstractContainerView, c::Component, a::AxisArray)
    a = _maybesubset(cv, c, a)
    ax = first(AxisArrays.axes(a))
    AxisArray(a, _wrapaxis(cv, tupleaxisvals(c, ax)))
end
function _wrapaxisarray(cv::AbstractContainerView, c::Component, a::Union{Float64,Symbol})
    ax = _maybesubset(cv, c, tupleaxisvals(c))
    AxisArray(fill(a, length(ax)), _wrapaxis(cv, ax))
end

devices(cv::AbstractContainerView) = values(cv.devices)
devices(cv::AbstractContainerView, T::Type{<:Device}) = (c for c = devices(cv) if isa(c, T))

axis(cv::AbstractContainerView) = _wrapaxis(cv, vcat((_maybesubset(cv, c, tupleaxisvals(c)) for c = devices(cv))...))
axis(cv::AbstractContainerView, T::Type{<:Component}) = axis(m[T])

# Base.cat(cv::AbstractContainerView, cs::Vector{<:Component}, as::Vector) = @consense(as, "Single values do not agree")
"""
    cat(containerview, components, attrvalues)

Combine the `attrvalues` for the `components` in a `containerview` to a single AxisArray.
"""
Base.cat(cv::AbstractContainerView, components::Vector{<:Component}, attrvalues::Vector) =
    cat((_wrapaxisarray(cv, c, a) for (c, a) = zip(components, attrvalues))...; dims=1)

mapcat(f::Function, cv::AbstractContainerView) = cat(cv, collect(devices(cv)), map(f, devices(cv)))

Base.get(cv::AbstractContainerView, i) = mapcat(c->get(c, i), cv)
Base.get(cv::AbstractContainerView, i, axes...) = WrappedArray(get(cv, i), axes...)

Base.getindex(cv::AbstractContainerView, i) = get(cv, i)
