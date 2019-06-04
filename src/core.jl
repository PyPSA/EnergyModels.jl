using Base.Iterators: flatten

abstract type Data end
abstract type ExpressionType end

abstract type Emission <: ExpressionType end
struct Cost <: ExpressionType end
struct CO2 <: Emission end

"Can be `add`ed to a JuMP model, must have an `objects` dictionary and a `class`"
abstract type Element end

abstract type AbstractEnergyModel end

"Represents a synchronuous zone. Induced by determine_subnetworks!(model)"
struct SubNetwork{T <: AbstractEnergyModel} <: Element
    model::T
    name::Symbol
    buses::Axis
end

"Connection points at which energy balance is upheld, has `axis` and `build`"
struct Bus{T <: AbstractEnergyModel} <: Element
    model::T
    name::Symbol
end

"Connected to at least one `Bus`. Additionally to `build` provides `nodalbalance` and `cost`"
abstract type Component <: Element end

mutable struct EnergyModel
    components::Dict{Symbol,Component}
    subnetworks::Dict{Symbol,SubNetwork{EnergyModel}}
    buses::Dict{Symbol,Bus{EnergyModel}}
    data::Data
    jumpmodel::Union{JuMP.AbstractModel,Nothing}
end


EnergyModel(filename::String; kwargs...) = EnergyModel(load(filename); kwargs...)
EnergyModel(data::Data) = load(EnergyModel(Dict{Symbol,Component}(), Dict{Symbol,SubNetwork}(), Dict{Symbol,Bus}(), data, nothing))

function load(m::EnergyModel)
    for T = modelelements(m.data), class = classes(m.data, T) push!(m, T(m, class)) end
    determine_subnetworks!(m)
    m
end

const Container = Union{SubNetwork{EnergyModel}, EnergyModel}

Base.push!(m::EnergyModel, c::Component) = (m.components[c.class] = c)
Base.push!(m::EnergyModel, c::SubNetwork) = (m.subnetworks[c.name] = c)
Base.push!(m::EnergyModel, c::Bus) = (m.buses[c.name] = c)

components(sn::SubNetwork) = components(model(sn))
components(c::EnergyModel) = values(c.components)
components(m::Container, T::Type{<:Component}) = (c for c = components(m) if isa(c, T))

buses(m::EnergyModel) = values(m.buses)
subnetworks(m::EnergyModel) = values(m.subnetworks)

JuMP.optimize!(m::EnergyModel; kwargs...) = optimize!(m.jumpmodel; kwargs...)

Base.show(io::IO, m::EnergyModel) = print(io, typeof(m), " with ", length(m.components), " components")
Base.show(io::IO, c::Component) = print(io, typeof(c), " for class ", c.class)
# function Base.show(io::IO, ::MIME"text/plain", c::Component)
#     println(io, c, " with ")
#     println(io, "* ", length(c.vars), " variables")
#     print(io, "* ", length(c.constrs), " constraints")
# end


jumpmodel(e::Element) = jumpmodel(model(e))
jumpmodel(m::EnergyModel) = m.jumpmodel

model(m::EnergyModel) = m
model(e::Element) = e.model

naming(c::Component) = c.class
naming(e::Element) = e.name
naming(e::Element, args...) = Symbol(naming(e), flatten(("::", a) for a=args)...)

Base.findall(pred::Base.Fix2{typeof(in), <:Axis}, c::Component) = intersect((findall(pred, c[attr]) for attr = busattributes(c))...)
Base.findall(pred::Base.Fix2{typeof(in), <:Axis}, c::Bus) = findall(pred, axis(c).val)

isvar(m::EnergyModel, e::Element, attr::Symbol) = isvar(m.data, e, attr)
isvar(e::Element, attr::Symbol) = isvar(model(e), e, attr)

function Base.getindex(m::EnergyModel, class::Symbol)
    for t = (:components, :buses, :subnetworks)
        haskey(getfield(m, t), class) && return getfield(m, t)[class]
    end
    throw(KeyError(class))
end
Base.getindex(m::EnergyModel, T::Type{<:Component}) = ContainerView(m, Dict(c.class=>c for c=components(m, T)))
Base.getindex(m::EnergyModel, ::Type{Bus}) = ContainerView(m, Dict(c.name=>c for c=buses(m)))

Base.getindex(sn::SubNetwork, T::Type{<:Component}) = SubContainerView(model(sn), Dict(c.class=>c for c=components(sn, T)), sn.buses)
Base.getindex(sn::SubNetwork, ::Type{Bus}) = SubContainerView(model(sn), Dict(c.name=>c for c=buses(model(sn))), sn.buses)

"""
    Base.get(e::Element, attr::Symbol)
    Base.get(e::Element, attr::Symbol, axes...)

For the element `e` get the attribute `attr`, which is either the

- JuMP variable or constraint, or the
- parameter data from the `Data` object for the `class` of the element

If `axes` are provided they must be `Symbol`s or AxisArrays.Axis objects, to
specify which dimensions need to be added flexibly, ie by wrapping in a
WrappedArray.

If you want a JuMP object or a parameter specifically, use `getjump` or
`getparam` instead.

# Examples

```julia
c = m[:onwind]

get(c, :p)   # gets the JuMP variable

get(c, :p_max_pu) # gets the plant availability
                  # (which might be static or a timeseries)

get(c, :p_max_pu, axis(c), :snapshots) # plant availability as timeseries
```
"""
function Base.get(e::Element, attr::Symbol)
    ret = getjump(e, attr)
    !isnothing(ret) ? ret : getparam(e, attr)
end
Base.get(e::Element, attr::Symbol, axes...) = WrappedArray(get(e, attr), axes...)

Base.getindex(e::Element, attr::Symbol) = get(e, attr)

getjump(e::Element, attr::Symbol) = get(e.objects, attr, nothing)
JuMP.getvalue(e::Element, attr::Symbol) = getvalue.(getjump(e, attr))
JuMP.getdual(e::Element, attr::Symbol) = getdual.(getjump(e, attr))
getparam(e::Element, attr::Symbol) = get(model(e).data, e, attr)

axis(m::EnergyModel, args...) = axis(m.data, args...)
axis(e::Element) = axis(model(e), e)
axis(e::Element, attr) = axis(model(e), attr)
axis(m::Container, T::Type{<:Element}) = axis(m[T])
axis(m::EnergyModel, T::Type{<:Element}) = axis(m[T])

# Could be specialized to not have to retrieve the whole axis (on the other
# hand, the axis should be cached, anyway)
Base.length(e::Element) = length(axis(e))
