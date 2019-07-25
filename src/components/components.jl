# Allow one and two-argument forms
cost(::EnergyModel, d::Device) = cost(d)
cost(::Device) = error("Not implemented")

expression(::Expression, m::EnergyModel, d::Device) = error("Not implemented")
expression(::Cost, m::EnergyModel, d::Device) = cost(d)
function expression(::NodalActivePower, m::EnergyModel, b::Bus)
    T = axis(m, :snapshots)
    buses = axis(m, b)
    p = AxisArray(zeros(AffExpr, length(buses), length(T)), buses, T)
    for d in devices(model(b))
        addtoexpr!(p, NodalActivePower(), m, d)
    end
    p
end

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
function addto!(jm::JuMP.AbstractModel, m::EnergyModel, d::Device)
    addto!(ModelView(jm, d), m, demote_formulation(m, d))
end

include("energymodel.jl")
include("subnetwork.jl")
include("bus.jl")

## OnePort

"Connected to exactly one `Bus`, determined by :bus attribute"
abstract type OnePort{DF<:DeviceFormulation} <: Device{DF} end
busattributes(d::OnePort) = (:bus,)

## Defaults for OnePort
cost(d::OnePort) = sum(d[:marginal_cost] .* d[:p]) + sum(d[:capital_cost] .* (d[:p_nom] .- getparam(d, :p_nom)))
function addtoexpr!(injection::AxisArray, ::NodalActivePower,
                    m::EnergyModel, d::OnePort)
    B, T = AxisArrays.axes(injection)
    D = axis(m, d)

    buses = get(d, :bus, D)
    p = get(d, :p, D, T)

    for (i, idx) in enumerate(indexin(buses, B)), t in T
        !isnothing(idx) && add_to_expression!(injection[idx,t], p[i,t])
    end
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
    p = d[:p]
    ((b,t)->p[b,t],   # :bus0
     (b,t)->-p[b,t])  # :bus1
end

function addtoexpr!(injection::AxisArray, ::NodalActivePower,
                    m::EnergyModel, d::Branch)
    B, T = AxisArrays.axes(injection)
    L = axis(m, d)

    bus0 = get(d, :bus0, L)
    bus1 = get(d, :bus1, L)
    p = get(d, :p, L, T)

    for (i, idx0, idx1) in zip(1:length(L), indexin(bus0, B), indexin(bus1, B)), t in T
        !isnothing(idx0) && add_to_expression!(injection[idx0,t], p[i,t])
        !isnothing(idx1) && add_to_expression!(injection[idx1,t], -1, p[i,t])
    end
end

include("link.jl")
include("passivebranch.jl")
