
mutable struct EnergyModel
    data::Data
    components::Dict{Symbol,Component}
    jump::Model
    cache::Dict{Symbol,Any}
end

abstract type Component end

def(::Component) = error("Not implemented")


abstract type OnePortComponent <: Component end

struct Generator <: OnePortComponent
    model::EnergyModel
    class::Symbol
    vars::Dict{Symbol, Any}
end

abstract type ExpressionType end
abstract type Cost <: ExpressionType end
abstract type Emission <: ExpressionType end
abstract type CO2 <: Emission end

jumpmodel(c::Component) = c.model.jump
expression(c::Component, ::ExpressionType) = error("Not implemented")
expression(c::Component, ::Cost) = cost(c)
cost(::Component) = error("Not implemented")

fqn(c::Component, attr::Symbol) = Symbol(typeof(c)), attr
getindex(c::Component, attr::Symbol) = haskey(c.vars, attr) ? c.vars[attr] : get(c.io, fqn(c, attr)..)



cost(c::Generator) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])

function def(c::Generator)
    m = jumpmodel(c)
    ns = names(c)
    ts = snapshots(c)

    if c[:p_nom_extendable]
        @variable c c[:p_nom_min] <= p_nom[n=ns] <= c[:p_nom_max]
    end

    @variable c c[:p_min_pu] .* c[:p_nom]  <= p[n=ns,t=ts] <= c[:p_max_pu] .* c[:p_nom]
end

function def(c::Generator, p_nom_extendable, p_nom_min, p_nom_max)

end







struct Load <: OnePortComponent
    class::Symbol
end
