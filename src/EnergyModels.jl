module EnergyModels

using Compat
using JuMP
using Gurobi
using AxisArrays

export @emvariable, @emconstraint, Component, EnergyModel, Generator

include("types.jl")
include("wrappedarray.jl")
include("macros.jl")
include("components.jl")
include("data.jl")

end # module
