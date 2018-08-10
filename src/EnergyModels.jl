__precompile__()

module EnergyModels

using Compat
using Destruct
using JuMP
using Gurobi
using AxisArrays
using LightGraphs
using MetaGraphs

include("compat.jl")
include("wrappedarray.jl")

include("core.jl")
include("registry.jl")
include("macros.jl")
include("graph.jl")
include("data.jl")
include("containerviews.jl")

include("elements.jl")
include("components/oneport.jl")
include("components/branch.jl")

export @emvariable, @emconstraint, build, solve, components, subnetworks, buses,
    push!, axis, graph, determine_subnetworks!, jumpmodel, Component,
    EnergyModel, SubNetwork, Bus, Line, Transformer, Link, Load, Generator,
    StorageUnit, Store

end # module
