mutable struct EnergyModel
    components::Dict{Symbol,Component}
    data::Data
    jump::Model
    cache::Dict{Symbol,Any}
end

build(::Model, ::Component) = error("Not implemented")
build(::Component) = error("Not implemented")

abstract type OnePortComponent <: Component end
abstract type AggregateComponent <: Component end
abstract type BranchComponent <: Component end
abstract type PassiveBranchComponent <: BranchComponent end

struct Generator <: OnePortComponent
    model::EnergyModel
    class::Symbol
    vars::Dict{Symbol, Any}
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

# function build(m::Model, c::Generator)
#     T = inds(c, :snapshots)
#     G = inds(c, :generators)

#     p_nom_min, p_nom_max = view(c, (:p_nom_min, :p_nom_max))
#     if c[:p_nom_extendable]
#         p_nom = registervar!(c, :p_nom, @variable m p_nom_min[t,g] <= [g=G] <= p_nom_max[t,g], G)
#         registervar!(c, :p, @variable m [g=G,t=T], (G, T))
#         registerconstr!(c, :p_lb, @constraint m p[g=G] >= p_min_pu[g,t] * p_nom[g], G)
#         registerconstr!(c, :p_ub, @constraint m p[g=G] <= p_max_pu[g,t] * p_nom[g], G)
#     else
#         p_nom = view(c, :p_nom)
#         registervar!(c, :p, @variable m p_min_pu[g,t] * p_nom[g] <= [g=G,t=T] <= p_max_pu[g,t] * p_nom[g], (G, T))
#     end
# end

# function build(m::Model, c::Generator)
#     G = indexset(c, :generators)
#     T = indexset(c, :snapshots)

#     if c[:p_nom_extendable]
#         @emvariable c c[:p_nom_min][g] <= p_nom[g=G] <= c[:p_nom_max][g]
#         @emvariable c p[g=G,t=T]
#         @emconstraint c balancing_lb[g=G,t=T] p[g,t] >= c[:p_min_pu][g,t] * c[:p_nom][g]
#         @emconstraint c balancing_ub[g=G,t=T] p[g,t] <= c[:p_max_pu][g,t] * c[:p_nom][g]
#     else
#         @emvariable c c[:p_min_pu][g,t] * c[:p_nom][g] <= p[g=G,t=T] <= c[:p_min_pu][g,t] * c[:p_nom][g]
#     end
# end


# struct QAxis{name,N} end

# struct Quantity{name,isvar,N,Ax}
#     Quantity{name,isvar,N,Ax}() where {name,isvar<:Bool,N,Ax<:Tuple{Vararg{QAxis,N}}} = new{name,isvar,N,Ax}()
# end

struct Load <: OnePortComponent
    class::Symbol
end

struct Bus <: Component
    class::Symbol
end

struct StorageUnit <: OnePortComponent
    class::Symbol
end

struct Store <: OnePortComponent
    class::Symbol
end
