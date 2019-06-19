# Allow one and two-argument forms
cost(::EnergyModel, d::Device) = cost(d)
cost(::Device) = error("Not implemented")

expression(d::Device, ::Expression) = error("Not implemented")
expression(d::Device, ::Cost) = cost(d)

"""
    addto!(jm::JuMP.AbstractModel, m::EnergyModel)
    addto!(jm::JuMP.AbstractModel, m::EnergyModel, d::Device)

Add the variables and constraints defining the energy model `m` to the jump
model `jm` and set the objective function. If an device `d` is specified, the
device is added alone.
"""
function addto! end

# Wraps the AbstractModel in a ModelView to store variables and constraints in m.objects[<class>]
function addto!(jm::JuMP.AbstractModel, m::EnergyModel, c::Component)
    addto!(ModelView(jm, c), m, c)
end

include("energymodel.jl")
include("subnetwork.jl")
include("bus.jl")

## OnePort

"Connected to exactly one `Bus`, determined by :bus attribute"
abstract type OnePort{DF<:DeviceFormulation} <: Device{DF} end
busattributes(d::OnePort) = (:bus,)

## Defaults for OnePort
cost(d::OnePort) = sum(d[:marginal_cost] .* AxisArray(d[:p])) + sum(d[:capital_cost] .* (AxisArray(d[:p_nom]) .- getparam(d, :p_nom)))
function p(d::OnePort)
    p = AxisArray(d[:p])
    ((o,t)->p[o,t],)
end

include("generator.jl")
include("storageunit.jl")
include("store.jl")
include("load.jl")

# Branch
"Connected to exactly two `Bus`es, determined by :bus0 and :bus1 attribute"
abstract type Branch{DF<:DeviceFormulation} <: Device{DF} end

# ActiveBranch
abstract type ActiveBranch{DF<:DeviceFormulation} <: Branch{DF} end

busattributes(d::Branch) = (:bus0, :bus1)
function p(d::Branch)
    p = AxisArray(d[:p])
    ((b,t)->p[b,t],   # :bus0
     (b,t)->-p[b,t])  # :bus1
end

include("link.jl")
include("passivebranch.jl")
