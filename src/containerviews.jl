abstract type AbstractContainerView{T} end

struct ContainerView{T} <: AbstractContainerView{T}
    model::EnergyModel
    components::Dict{Symbol,T}
end

struct SubContainerView{T} <: AbstractContainerView{T}
    model::EnergyModel
    components::Dict{Symbol,T}
    buses::Axis
end

tupleaxisvals(c::Bus, A) = A.val
tupleaxisvals(c) = tupleaxisvals(c, axis(c))
tupleaxisvals(c, A) = AxisArrays.CategoricalVector(tuple.(c.class, A.val))

_maybesubset(cv::ContainerView, c, A) = A
_maybesubset(cv::SubContainerView, e::Element, A) = A[findall(in(cv.buses), e), ntuple(i->:,ndims(A)-1)...]

_viewaxis(cv::AbstractContainerView{T}, a) where T = Axis{Symbol(naming(T))}(a)

components(cv::AbstractContainerView) = values(cv.components)
components(cv::AbstractContainerView, T::Type{<:Component}) = (c for c = components(cv) if isa(c, T))

axis(cv::AbstractContainerView) = _viewaxis(cv, vcat((_maybesubset(cv, c, tupleaxisvals(c)) for c = components(cv))...))
axis(cv::AbstractContainerView, T::Type{<:Element}) = axis(m[T])

Base.cat(cv::AbstractContainerView, cs::Vector{<:Element}, as::Vector) = @consense(as, "Single values do not agree")
function Base.cat(cv::AbstractContainerView, cs::Vector{<:Element}, as::Vector{<:AxisArray})
    _viewaxisarray(c, a) = AxisArray(a, _viewaxis(cv, tupleaxisvals(c, first(AxisArrays.axes(a)))))
    cat((_viewaxisarray(c, _maybesubset(cv, c, a)) for (c, a) = zip(cs, as))...; dims=1)
end

mapcat(f::Function, cv::AbstractContainerView) = cat(cv, collect(components(cv)), map(f, components(cv)))

# TODO The AxisArray that is spliced in here is a mild hack!
Base.get(cv::AbstractContainerView, i) = mapcat(c->AxisArray(get(c, i)), cv)
Base.get(cv::AbstractContainerView, i, axes...) = WrappedArray(get(cv, i), axes...)

Base.getindex(cv::AbstractContainerView, i) = get(cv, i)
