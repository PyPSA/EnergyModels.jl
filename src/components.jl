module Components

export
  EnergyModel, Bus, Generator, Load, StorageUnit, Store, Link, Line,
  build, indexset, view, jumpmodel, expression

mutable struct EnergyModel
  components::Dict{Symbol,Component}
  data::Data
  jump::Model
  cache::Dict{Symbol,Any}
end

build(::Model, ::Component) = error("Not implemented")
build(::Component) = error("Not implemented")

abstract type AggregateComponent <: Component end

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
struct SubNetwork <: AggregateComponent
  model::EnergyModel
  class::Symbol
  vars::Dict{Symbol, Any}
  constrs::Dict{Symbol, Any}
end


abstract type ExpressionType end
abstract type Emission <: ExpressionType end
struct Cost <: ExpressionType end
struct CO2 <: Emission end

jumpmodel(c::Component) = c.model.jump
expression(c::Component, ::ExpressionType) = error("Not implemented")
expression(c::Component, ::Cost) = cost(c)
cost(::Component) = error("Not implemented")

struct IndexSet{T} <: AbstractArray{T, 1}
  name::Symbol
  values::Vector{T}
end

Base.size(set::IndexSet) = Base.size(set.values)
Base.IndexStyle(::Type{<:IndexSet{T}}) where T = Base.IndexStyle(Vector{T})
Base.getindex(set::IndexSet, i) = set.values[i]

Base.getindex(c::Component, attr::Symbol) = haskey(c.vars, attr) ? c.vars[attr] : get(c.model.data, c, c.class, attr)

# We should use the sets information to make sure, we're getting the correct bit
view(c::Component, attr::Symbol, sets::Vector{IndexSet{T}}) where T = c[attr]
indexset(c::Component, attr::Symbol) = IndexSet(attr, c[attr])

cost(m::Model, c::Generator) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])

function build(c::Generator)
  G = indexset(c, :generators)
  T = indexset(c, :snapshots)

  if c[:p_nom_extendable]
      @emvariable c c[:p_nom_min][g] <= p_nom[g=G] <= c[:p_nom_max][g]
      @emvariable c p[g=G,t=T]
      @emconstraint c balancing_lb[g=G,t=T] p[g,t] >= c[:p_min_pu][g,t] * c[:p_nom][g]
      @emconstraint c balancing_ub[g=G,t=T] p[g,t] <= c[:p_max_pu][g,t] * c[:p_nom][g]
  else
      @emvariable c c[:p_min_pu][g,t] * c[:p_nom][g] <= p[g=G,t=T] <= c[:p_min_pu][g,t] * c[:p_nom][g]
  end
end


end # of module Components
