using Base.Iterators: flatten

abstract type Data end
abstract type ExpressionType end

abstract type Emission <: ExpressionType end
struct Cost <: ExpressionType end
struct CO2 <: Emission end

"Can be `add`ed to a JuMP model, must have an `objects` dictionary and a `class`"
abstract type Component end

abstract type AbstractEnergyModel end

"Represents a synchronuous zone. Induced by determine_subnetworks!(model)"
struct SubNetwork{T <: AbstractEnergyModel} <: Component
    model::T
    class::Symbol
    buses::Axis
    objects::Dict{Symbol,Any}
end

"Connection points at which energy balance is upheld, has `axis` and `addto!`"
struct Bus{T <: AbstractEnergyModel} <: Component
    model::T
    class::Symbol
    objects::Dict{Symbol,Any}
end

"Connected to at least one `Bus`. Additionally to `addto!` provides `p` and `cost`"
abstract type Device <: Component end

mutable struct EnergyModel <: AbstractEnergyModel
    devices::Dict{Symbol,Device}
    subnetworks::Dict{Symbol,SubNetwork{EnergyModel}}
    buses::Dict{Symbol,Bus{EnergyModel}}
    data::Data
    jumpmodel::Union{JuMP.AbstractModel,Nothing}
end


EnergyModel(filename::String; kwargs...) = EnergyModel(load(filename); kwargs...)
EnergyModel(data::Data) = load(EnergyModel(Dict{Symbol,Device}(), Dict{Symbol,SubNetwork}(), Dict{Symbol,Bus}(), data, nothing))

function load(m::EnergyModel)
    for T in modelcomponents(m.data), class in classes(m.data, T)
        push!(m, T(m, class, Dict{Symbol}{Any}()))
    end
    determine_subnetworks!(m)
    m
end

Base.push!(m::EnergyModel, d::Device) = (m.devices[d.class] = d)
Base.push!(m::EnergyModel, c::SubNetwork) = (m.subnetworks[c.class] = c)
Base.push!(m::EnergyModel, c::Bus) = (m.buses[c.class] = c)

devices(m::EnergyModel) = values(m.devices)
devices(sn::SubNetwork) = devices(model(sn))

devices(m::EnergyModel, T::Type{<:Device}) = (d for d = devices(m) if isa(d, T))
devices(sn::SubNetwork, T::Type{<:Device}) = (d for d = devices(sn) if isa(d, T))

buses(m::EnergyModel) = values(m.buses)
subnetworks(m::EnergyModel) = values(m.subnetworks)

JuMP.optimize!(m::EnergyModel; kwargs...) = optimize!(m.jumpmodel; kwargs...)

Base.show(io::IO, m::EnergyModel) = print(io, typeof(m), " with ", length(m.devices), " devices")
Base.show(io::IO, d::Device) = print(io, typeof(d), " for class ", d.class)
# function Base.show(io::IO, ::MIME"text/plain", d::Device)
#     println(io, d, " with ")
#     println(io, "* ", length(d.vars), " variables")
#     print(io, "* ", length(d.constrs), " constraints")
# end

model(m::EnergyModel) = m
model(c::Component) = c.model

naming(c::Component) = c.class
naming(c::Component, args...) = Symbol(naming(c), flatten((:(::), a) for a=args)...)

Base.findall(pred::Base.Fix2{typeof(in), <:Axis}, d::Device) = intersect((findall(pred, d[attr]) for attr = busattributes(d))...)
Base.findall(pred::Base.Fix2{typeof(in), <:Axis}, c::Bus) = findall(pred, axis(c).val)

isvar(m::EnergyModel, c::Component, attr::Symbol) = isvar(m.data, c, attr)
isvar(c::Component, attr::Symbol) = isvar(model(c), c, attr)

function Base.getindex(m::EnergyModel, class::Symbol)
    for t = (:devices, :buses, :subnetworks)
        haskey(getfield(m, t), class) && return getfield(m, t)[class]
    end
    throw(KeyError(class))
end
Base.getindex(m::EnergyModel, T::Type{<:Device}) = ContainerView(m, Dict(d.class=>d for d=devices(m, T)))
Base.getindex(m::EnergyModel, ::Type{Bus}) = ContainerView(m, Dict(d.class=>d for d=buses(m)))

Base.getindex(sn::SubNetwork, T::Type{<:Device}) = SubContainerView(model(sn), Dict(d.class=>d for d=devices(sn, T)), sn.buses)
Base.getindex(sn::SubNetwork, ::Type{Bus}) = SubContainerView(model(sn), Dict(d.class=>d for d=buses(model(sn))), sn.buses)

"""
    Base.get(c::Component, attr::Symbol)
    Base.get(c::Component, attr::Symbol, axes...)

For the component `c` get the attribute `attr`, which is either the

- JuMP variable or constraint, or the
- parameter data from the `Data` object for the `class` of the component

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
function Base.get(c::Component, attr::Symbol)
    ret = getjump(c, attr)
    !isnothing(ret) ? ret : getparam(c, attr)
end
Base.get(c::Component, attr::Symbol, axes...) = WrappedArray(get(c, attr), axes...)

Base.getindex(c::Component, attr::Symbol) = get(c, attr)

getjump(c::Component, attr::Symbol) = get(c.objects, attr, nothing)
JuMP.getvalue(c::Component, attr::Symbol) = getvalue.(getjump(c, attr))
JuMP.getdual(c::Component, attr::Symbol) = getdual.(getjump(c, attr))
getparam(c::Component, attr::Symbol) = get(model(c).data, c, attr)

axis(m::EnergyModel, args...) = axis(m.data, args...)
axis(c::Component) = axis(model(c), c)
axis(c::Component, attr) = axis(model(c), attr)

axis(m::EnergyModel, T::Type{<:Component}) = axis(m[T])
axis(m::SubNetwork, T::Type{<:Component}) = axis(m[T])

# Could be specialized to not have to retrieve the whole axis (on the other
# hand, the axis should be cached, anyway)
Base.length(c::Component) = length(axis(c))
