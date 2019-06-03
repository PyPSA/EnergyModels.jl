__precompile__()

module EnergyModels

using Compat
using Destruct
using JuMP
using Gurobi
using AxisArrays
using LightGraphs
using MetaGraphs
using DataFrames
using Logging
using SparseArrays

# using Memento

# # Create our module level logger (this will get precompiled)
# const logger = getlogger(@__MODULE__)

# # Register the module level logger at runtime so that folks can access the logger via `get_logger(MyModule)`
# # NOTE: If this line is not included then the precompiled `MyModule.LOGGER` won't be registered at runtime.
# function __init__()
#     Memento.register(logger)
# end


include("core.jl")
include("compat.jl")
include("modelview.jl")
include("wrappedarray.jl")
include("registry.jl")
include("graph.jl")
include("data.jl")
include("containerviews.jl")

include("elements.jl")
include("components/oneport.jl")
include("components/branch.jl")

export @emvariable, @emconstraint, build, solve, components, subnetworks, buses,
    push!, axis, graph, determine_subnetworks!, jumpmodel, getvalue, getdual,
    getparam

# Components
export EnergyModel, SubNetwork, Component, Bus, Line, Transformer, Link,
    Load, Generator, StorageUnit, Store, Branch, PassiveBranch, ActiveBranch,
    OnePort

end # module
