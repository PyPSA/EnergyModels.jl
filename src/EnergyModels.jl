__precompile__()

module EnergyModels

using CSV
using Destruct
using JuMP
using AxisArrays
using LightGraphs
using MetaGraphs
using DataFrames
using Logging
using SparseArrays

import PowerModels
const PM = PowerModels

import MathOptInterface
const MOI = MathOptInterface

using Base.Iterators: flatten

# using Memento

# # Create our module level logger (this will get precompiled)
# const logger = getlogger(@__MODULE__)

# # Register the module level logger at runtime so that folks can access the logger via `get_logger(MyModule)`
# # NOTE: If this line is not included then the precompiled `MyModule.LOGGER` won't be registered at runtime.
# function __init__()
#     Memento.register(logger)
# end


include("abstracttypes.jl")
include("core.jl")

include("formulation.jl")
include("compat.jl")
include("macros.jl")
include("modelview.jl")
include("wrappedarray.jl")
include("registry.jl")
include("graph.jl")
include("data.jl")
include("containerviews.jl")

include("components/components.jl")

export addto!, build!, optimize!, devices, subnetworks, buses, push!, axis,
    graph, determine_subnetworks!, jumpmodel, get, getjump, getvalue, getdual,
    getparam

# Components and Devices
export EnergyModel, SubNetwork, Device, Bus, Line, Transformer, Link,
    Load, Generator, StorageUnit, Store, Branch, PassiveBranch, ActiveBranch,
    OnePort

end # module
