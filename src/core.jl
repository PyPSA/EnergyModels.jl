"Connection points at which energy balance is upheld, has `axis` and `addto!`"
struct Bus <: Component
    model::AbstractEnergyModel
    class::Symbol
end

"Represents a synchronuous zone. Induced by determine_subnetworks!(model)"
struct SubNetwork <: Component
    model::AbstractEnergyModel
    class::Symbol
    buses::Axis
end

mutable struct EnergyModel{MT <: ModelType, TF <: PM.AbstractPowerFormulation} <: AbstractEnergyModel
    devices::Dict{Symbol,Device}
    subnetworks::Dict{Symbol,SubNetwork}
    buses::Dict{Symbol,Bus}
    axes::Dict{Symbol,Axis}
    data::AbstractData
    parent::Union{AbstractEnergyModel,Nothing}
    jumpmodel::Union{JuMP.AbstractModel,Nothing}
    jumpnames::Bool
    jumpobjects::Dict{Symbol,Dict{Symbol}{Any}}
end

ensure_optimizerfactory(opt::JuMP.OptimizerFactory) = opt
ensure_optimizerfactory(::Type{T}) where T <: MOI.AbstractOptimizer = with_optimizer(T)

function EnergyModel(::Type{MT}, ::Type{TF}, data::AbstractData;
                     parent=nothing, jumpmodel=nothing, jumpnames=true, optimizer=nothing) where
        {MT <: ModelType, TF <: PM.AbstractPowerFormulation}

    if isnothing(jumpmodel)
        jumpmodel = isnothing(optimizer) ? JuMP.Model() : JuMP.Model(ensure_optimizerfactory(optimizer))
    end

    EnergyModel{MT,TF}(Dict{Symbol,Device}(),
                       Dict{Symbol,SubNetwork}(),
                       Dict{Symbol,Bus}(),
                       Dict{Symbol,Axis}(),
                       data,
                       parent,
                       jumpmodel,
                       jumpnames,
                       Dict{Symbol, Dict{Symbol}{Any}}())
end

function EnergyModel(other::EnergyModel{MT,TF};
                     modeltype=nothing, transmissionform=nothing,
                     jumpnames=nothing, kwargs...) where
        {MT <: ModelType, TF <: PM.AbstractPowerFormulation}

    if isnothing(modeltype) modeltype = MT end
    if isnothing(transmissionform) transmissionform = TF end

    kwargs = Dict{Symbol,Any}(kwargs)
    get!(kwargs, :jumpnames, other.jumpnames)

    model = EnergyModel(modeltype, transmissionform, other.data; kwargs...)

    merge!(model.buses, Dict(k=>withmodel(v, model) for (k,v) in other.buses))
    merge!(model.devices, Dict(k=>withmodel(v, model) for (k,v) in other.devices))
    merge!(model.axes, other.axes)

    determine_subnetworks!(model)

    model
end

EnergyModel(; kwargs...) = EnergyModel(Data(); kwargs...)
EnergyModel(filename; kwargs...) = EnergyModel(load(filename); kwargs...)
EnergyModel(data::AbstractData; modeltype=ExpansionModel, transmissionform=PM.DCPlosslessForm, kwargs...) = load(EnergyModel(modeltype, transmissionform, data; kwargs...))

function load(m::EnergyModel)
    for (class, T) in components(m.data)
        push!(m, T(m, class))
    end
    determine_subnetworks!(m)
    m
end

Base.push!(m::EnergyModel, d::Device) = (m.devices[d.class] = d)
Base.push!(m::EnergyModel, c::SubNetwork) = (m.subnetworks[c.class] = c)
Base.push!(m::EnergyModel, c::Bus) = (m.buses[c.class] = c)
Base.push!(m::EnergyModel, ax::Axis{name}) where name = (m.axes[name] = ax)

set_snapshots!(m::EnergyModel, snapshots::AbstractArray) =
    push!(m, Axis{:snapshots}(collect(snapshots)))

with_model(d::Device, DF::DeviceFormulation) = with_formulation(d, DF)
with_model(d::Device, ::Type{T}) where T <: Device = model()
function set_formulation!(m::EnergyModel, changes::Pair...)
    for (selector, newmodel) in changes
        for d in devices(m, selector)
            push!(m, with_formulation(d, newmodel))
        end
    end
end

store_results!(m::EnergyModel) = store_results!(m.data, m)

devices(m::EnergyModel) = values(m.devices)
devices(sn::SubNetwork) = devices(model(sn))

devices(m::EnergyModel, T::Type{<:Device}) = (d for d = devices(m) if isa(d, T))
devices(sn::SubNetwork, T::Type{<:Device}) = (d for d = devices(sn) if isa(d, T))

buses(m::EnergyModel) = values(m.buses)
subnetworks(m::EnergyModel) = values(m.subnetworks)

JuMP.optimize!(m::EnergyModel; kwargs...) = optimize!(m.jumpmodel; kwargs...)

Base.show(io::IO, m::EnergyModel) = print(io, typeof(m), " with ", length(axis(m, Bus)), " buses and ", length(m.devices), " devices")
Base.show(io::IO, c::Component) = print(io, typeof(c), "($(c.class))")
function Base.show(io::IO, c::Device{DF}) where DF <: DeviceFormulation
    print(io, typeof(c), "($(c.class))")
    demoted_DF = demote_formulation(modeltype(model(c)), DF)
    demoted_DF != DF && print(io, " (demoted to $demoted_DF)")
end


# function Base.show(io::IO, ::MIME"text/plain", d::Device)
#     println(io, d, " with ")
#     println(io, "* ", length(d.vars), " variables")
#     print(io, "* ", length(d.constrs), " constraints")
# end

model(m::EnergyModel) = m
model(c::Component) = c.model
modeltype(m::EnergyModel{MT,TF}) where {MT <: ModelType, TF <: PM.AbstractPowerFormulation} = MT
transmissionform(m::EnergyModel{MT,TF}) where {MT <: ModelType, TF <: PM.AbstractPowerFormulation} = TF

naming(c::Component) = c.class
naming(c::Component, args...) = Symbol(naming(c), flatten((:(::), a) for a=args)...)

Base.findall(pred::Base.Fix2{typeof(in), <:Axis}, d::Device) = intersect((findall(pred, get(d, attr, axis(d))) for attr = busattributes(d))...)
Base.findall(pred::Base.Fix2{typeof(in), <:Axis}, c::Bus) = findall(pred, axis(c).val)

function Base.getindex(m::EnergyModel, class::Symbol)
    for t = (:devices, :buses, :subnetworks)
        haskey(getfield(m, t), class) && return getfield(m, t)[class]
    end
    throw(KeyError(class))
end
Base.getindex(m::EnergyModel, ::Type{T}) where T <: Device = ContainerView(m, Dict{Symbol,T}(d.class=>d for d=devices(m, T)))
Base.getindex(m::EnergyModel, ::Type{Bus}) = ContainerView(m, Dict{Symbol,Bus}(d.class=>d for d=buses(m)))

Base.getindex(sn::SubNetwork, ::Type{T}) where T <: Device = SubContainerView(model(sn), Dict{Symbol,T}(d.class=>d for d=devices(sn, T)), sn.buses)
Base.getindex(sn::SubNetwork, ::Type{Bus}) = SubContainerView(model(sn), Dict{Symbol,Bus}(d.class=>d for d=buses(model(sn))), sn.buses)

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

getjump(m::EnergyModel, c::Component, attr::Symbol) = getjump(m, c.class, attr)
function getjump(m::EnergyModel, class::Symbol, attr::Symbol)
    ret = get(get!(m.jumpobjects, class, Dict{Symbol}{Any}()), attr, nothing)
    if !isnothing(ret)
        AxisArray(ret)
    elseif !isnothing(m.parent)
        getjump(m.parent, class, attr)
    else
        nothing
    end
end
getjump(c::Component, attr::Symbol) = getjump(model(c), c, attr)
JuMP.getvalue(c::Component, attr::Symbol) = getvalue.(getjump(c, attr))
JuMP.getdual(c::Component, attr::Symbol) = getdual.(getjump(c, attr))
getparam(c::Component, attr::Symbol) = get(model(c).data, c, attr)

function axis(m::EnergyModel, name::Symbol)
    ax = get(m.axes, name, nothing)
    !isnothing(ax) ? ax : axis(m.data, name)
end
axis(m::EnergyModel, args...) = axis(m.data, args...)
axis(c::Component) = axis(model(c), c)
axis(c::Component, attr) = axis(model(c), attr)

axis(m::EnergyModel, T::Type{<:Component}) = axis(m[T])
axis(m::SubNetwork, T::Type{<:Component}) = axis(m[T])

# Could be specialized to not have to retrieve the whole axis (on the other
# hand, the axis should be cached, anyway)
Base.length(c::Component) = length(axis(c))

issimple(c::Component) = issimple(model(c).data, naming(c))
issimple(::SubNetwork) = false

withmodel(c::T, m::EnergyModel) where T <: Component = T(m, c.class)

add!(m::EnergyModel, ::Type{T}, class::Symbol; kwargs...) where T <: Component = add!(m, T, Axis{class}([class]); kwargs...)
add!(m::EnergyModel, ::Type{T}, class::Symbol, names; kwargs...) where T <: Component = add!(m, T, Axis{class}(names); kwargs...)
function add!(m::EnergyModel, ::Type{T}, ax::Axis{class}; suffix=nothing, parameters...) where {class, T <: Component}
    if !isnothing(suffix)
        ax = Axis{class}(Symbol.(ax.val, suffix))
    end

    push!(m.data, T, ax; parameters...)
    push!(m, T(m, class))
end
