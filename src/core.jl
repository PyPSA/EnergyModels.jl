using Base.Iterators: flatten

abstract type Data end
abstract type ExpressionType end

abstract type Emission <: ExpressionType end
struct Cost <: ExpressionType end
struct CO2 <: Emission end

"Can be `build` into a linked `jumpmodel`, they CAN have `variables` and `constraints`"
abstract type ModelElement end

"Holds `components` and provides getindex access by their class or a `ContainerView`"
abstract type Container <: ModelElement end

abstract type AbstractModel <: Container end

"Represents a synchronuous zone. Induced by determine_subnetworks!(model)"
struct SubNetwork{T <: AbstractModel} <: Container
    model::T
    name::Symbol
    buses::Axis
end

"Connection points at which energy balance is upheld, has `axis` and `build`"
struct Bus{T <: AbstractModel} <: ModelElement
    model::T
    name::Symbol
end

"Connected to at least one `Bus`. Additionally to `build` provides `nodalbalance` and `cost`"
abstract type Component <: ModelElement end

mutable struct EnergyModel <: AbstractModel
    components::Dict{Symbol,Component}
    subnetworks::Dict{Symbol,SubNetwork{EnergyModel}}
    buses::Dict{Symbol,Bus{EnergyModel}}
    data::Data
    jump::Model
end


EnergyModel(filename::String; kwargs...) = EnergyModel(load(filename); kwargs...)
EnergyModel(data::Data; solver=GurobiSolver()) = load(EnergyModel(Dict{Symbol,Component}(), Dict{Symbol,SubNetwork}(), Dict{Symbol,Bus}(), data, Model(solver=solver)))

function load(m::EnergyModel)
    for T = modelelements(m.data), class = classes(m.data, T) push!(m, T(m, class)) end
    determine_subnetworks!(m)
    m
end

Base.push!(m::EnergyModel, c::Component) = (m.components[c.class] = c)
Base.push!(m::EnergyModel, c::SubNetwork) = (m.subnetworks[c.name] = c)
Base.push!(m::EnergyModel, c::Bus) = (m.buses[c.name] = c)

components(sn::SubNetwork) = components(model(sn))
components(c::Container) = values(c.components)
components(m::Container, T::Type{<:Component}) = (c for c = components(m) if isa(c, T))
# containerview(m::EnergyModel) = ContainerView()
# containerview(sn::SubNetwork) = SubContainerView()

buses(m::EnergyModel) = values(m.buses)
subnetworks(m::EnergyModel) = values(m.subnetworks)

JuMP.solve(m::EnergyModel; kwargs...) = solve(m.jump; kwargs...)

Base.show(io::IO, m::EnergyModel) = print(io, typeof(m), " with ", length(m.components), " components")
Base.show(io::IO, c::Component) = print(io, typeof(c), " for class ", c.class)
# function Base.show(io::IO, ::MIME"text/plain", c::Component)
#     println(io, c, " with ")
#     println(io, "* ", length(c.vars), " variables")
#     print(io, "* ", length(c.constrs), " constraints")
# end


jumpmodel(e::ModelElement) = jumpmodel(model(e))
jumpmodel(m::EnergyModel) = m.jump

model(m::EnergyModel) = m
model(e::ModelElement) = e.model

naming(c::Component) = c.class
naming(e::ModelElement) = e.name
naming(e::ModelElement, args...) = Symbol(naming(e), flatten(("::", a) for a=args)...)

indicesinbuses(c::Component, buses) = intersect((findin(c[attr], buses.val) for attr = busattributes(c))...)
indicesinbuses(c::Bus, buses) = findin(axis(c), buses.val)

isvar(m::EnergyModel, e::ModelElement, attr::Symbol) = isvar(m.data, e, attr)
isvar(e::ModelElement, attr::Symbol) = isvar(model(e), e, attr)

Base.getindex(e::ModelElement, attr::Symbol) = (r = getjump(e, attr)) !== nothing ? r : getparam(e, attr)

function getjump(e::ModelElement, attr::Symbol)
    jm = jumpmodel(e)
    name = naming(e, attr)
    haskey(jm.objDict, name) ? AxisArray(jm[name]) : nothing
end
getvariable(e::ModelElement, attr::Symbol) = getjump(e, attr)
getvalue(e::ModelElement, attr::Symbol) = getvalue(getjump(e, attr))
getdual(e::ModelElement, attr::Symbol) = getdual(getjump(e, attr))
getparam(e::ModelElement, attr::Symbol) = get(model(e).data, e, attr)

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

Base.view(e::ModelElement, attr::Symbol, axes) = WrappedArray(e[attr], axes...)

axis(m::EnergyModel, args...) = axis(m.data, args...)
axis(e::ModelElement) = axis(model(e), e)
axis(e::ModelElement, attr) = axis(model(e), attr)
axis(m::Container, T::Type{<:ModelElement}) = axis(m[T])
axis(m::EnergyModel, T::Type{<:ModelElement}) = axis(m[T])

# Could be specialized to not have to retrieve the whole axis (on the other
# hand, the axis should be cached, anyway)
Base.length(e::ModelElement) = length(axis(e))

