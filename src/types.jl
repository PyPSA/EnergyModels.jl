abstract type Data end
abstract type Component end
abstract type ExpressionType end

abstract type Emission <: ExpressionType end
struct Cost <: ExpressionType end
struct CO2 <: Emission end

abstract type Container end
abstract type AbstractModel <: Container end

# There are no cyclic dependencies in Julia
struct SubNetwork{T <: AbstractModel} <: Container
    model::T
    buses::Axis
end

struct EnergyModel <: AbstractModel
    components::Dict{Symbol,Component}
    subnetworks::Vector{SubNetwork{EnergyModel}}
    data::Data
    jump::Model
    # cache::Dict{Symbol,Any}
end

struct Bus <: Component
    model::EnergyModel
    class::Symbol
end

# OnePort
abstract type OnePort <: Component end
for component = (:Generator, :Load, :StorageUnit, :Store)
    @eval begin
        struct $component <: OnePort
            model::EnergyModel
            class::Symbol
        end
    end
end

# Branch
abstract type Branch <: Component end

# ActiveBranch
abstract type ActiveBranch <: Branch end
struct Link <: ActiveBranch
    model::EnergyModel
    class::Symbol
end

# PassiveBranch
abstract type PassiveBranch<: Branch end
for component = (:Line, :Transformer)
    @eval begin
        struct $component <: PassiveBranch
            model::EnergyModel
            class::Symbol
        end
    end
end

abstract type AbstractContainerView{T} <: Container end

struct ContainerView{T} <: AbstractContainerView{T}
    model::EnergyModel
    components::Vector{T}
end

struct SubContainerView{T} <: AbstractContainerView{T}
    model::EnergyModel
    components::Vector{T}
    buses::Axis
end
