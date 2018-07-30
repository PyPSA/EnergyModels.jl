__precompile__()

module EnergyModels

using Compat
using JuMP
using Gurobi
using AxisArrays
using LightGraphs

export @emvariable, @emconstraint, Component, EnergyModel, Generator

include("types.jl")
include("compat.jl")
include("wrappedarray.jl")
include("macros.jl")
include("components.jl")
include("data.jl")

end # module
