abstract type Data end
abstract type Component end
abstract type ExpressionType end

abstract type Emission <: ExpressionType end
struct Cost <: ExpressionType end
struct CO2 <: Emission end

mutable struct EnergyModel
  components::Dict{Symbol,Component}
  data::Data
  jump::Model
  # cache::Dict{Symbol,Any}
end

struct Bus <: Component
  model::EnergyModel
  class::Symbol
  vars::Dict{Symbol, Any}
  constrs::Dict{Symbol, Any}
end

# OnePortComponents
abstract type OnePortComponent <: Component end
for component = (:Generator, :Load, :StorageUnit, :Store)
  @eval begin
      struct $component <: OnePortComponent
          model::EnergyModel
          class::Symbol
          vars::Dict{Symbol, Any}
          constrs::Dict{Symbol, Any}
      end
  end
end

# BranchComponents
abstract type BranchComponent <: Component end

# ActiveBranchComponents
abstract type ActiveBranchComponent <: BranchComponent end
struct Link <: ActiveBranchComponent
  model::EnergyModel
  class::Symbol
  vars::Dict{Symbol, Any}
  constrs::Dict{Symbol, Any}
end

# PassiveBranchComponent
abstract type PassiveBranchComponent <: BranchComponent end
for component = (:Line, :Transformer)
  @eval begin
      struct $component <: PassiveBranchComponent
          model::EnergyModel
          class::Symbol
          vars::Dict{Symbol, Any}
          constrs::Dict{Symbol, Any}
      end
  end
end

# AggregateComponents
abstract type AggregateComponent <: Component end
struct SubNetwork <: AggregateComponent
  model::EnergyModel
  class::Symbol
  vars::Dict{Symbol, Any}
  constrs::Dict{Symbol, Any}
end
