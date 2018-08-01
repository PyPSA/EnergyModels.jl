# __precompile__()

module EnergyModels

using Compat
using Destruct
using NamedTuples
using JuMP
using Gurobi
using AxisArrays
using LightGraphs
using MetaGraphs

include("types.jl")

export
    @emvariable, @emconstraint, components, push!,
    axis, graph, determine_subnetworks!,
    Component, EnergyModel, SubNetwork,
    Bus, Line, Link,
    Generator, StorageUnit, Store

include("compat.jl")
include("wrappedarray.jl")
include("graph.jl")
include("macros.jl")
include("components.jl")
include("data.jl")

end # module
