module EnergyModels

using Compat
using JuMP

export @emvariable, @emconstraint, Component, EnergyModel, Generator

include("types.jl")
include("macros.jl")
include("data.jl")
include("struct.jl")

end # module
