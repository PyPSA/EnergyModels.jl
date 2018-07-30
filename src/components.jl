EnergyModel(filename::String; kwargs...) = EnergyModel(load(filename); kwargs...)
EnergyModel(data::Data; solver=GurobiSolver()) = load(EnergyModel(Dict{Symbol,Component}(), data, Model(solver=solver)))

function load(m::EnergyModel)
    for T = components(m.data), class = classes(m.data, T)
        m.components[Symbol(string(naming(T), "::", class))] = T(m, class)
    end
    m
end

JuMP.solve(m::EnergyModel; kwargs...) = solve(m.jump; kwargs...)

Base.show(io::IO, c::Component) = print(io, typeof(c), " for class ", c.class)
# function Base.show(io::IO, ::MIME"text/plain", c::Component)
#     println(io, c, " with ")
#     println(io, "* ", length(c.vars), " variables")
#     print(io, "* ", length(c.constrs), " constraints")
# end

isvar(c::Component, attr::Symbol) = isvar(c.model.data, c, c.class, attr)

build(::Model, ::Component) = error("Not implemented")
build(::Component) = error("Not implemented")

jumpmodel(c::Component) = c.model.jump
jumpmodel(m::EnergyModel) = m.jump

expression(c::Component, ::ExpressionType) = error("Not implemented")
expression(c::Component, ::Cost) = cost(c)
cost(::Component) = error("Not implemented")

cost(::EnergyModel, c::Component) = cost(c) # Allow one and two-argument forms
build(::EnergyModel, c::Component) = build(c)

function Base.getindex(c::Component, attr::Symbol)
    name = naming(Symbol, c, c.class, attr)
    m = jumpmodel(c)
    haskey(m.objDict, name) ? m.objDict[name] : get(c.model.data, c, c.class, attr)
end

# TODO probably introduce an indirection so that the array is only wrapped if extra axes are needed
view(c::Component, attr::Symbol, axes) = WrappedArray(c[attr], axes...)

axis(m::EnergyModel, args...) = axis(m.data, args...)
axis(c::Component) = axis(c.model, c, c.class)
axis(c::Component, attr) = axis(c.model, attr)

"""
    relativeaxis(args...)

Return an Iterator which allows for relative operations like i-1 or i+1 to refer
to the next index. Beware: It is slower by several orders of magnitude!
"""
relativeaxis(args...) = RelativeIterator(axis(args...))

## Definition of a generator

cost(c::OnePortComponent) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])

nodal_balance(c::OnePortComponent) = c[:p]
function build(m::EnergyModel, c::Generator)
    T = axis(m, :snapshots)
    G = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][g] <= p_nom[g=G] <= c[:p_nom_max][g]
        @emvariable c p[g=G,t=T]
        @emconstraint c p_lower[g=G,t=T] c[:p][g,t] >= c[:p_min_pu][g,t] * c[:p_nom][g]
        @emconstraint c p_upper[g=G,t=T] c[:p][g,t] <= c[:p_max_pu][g,t] * c[:p_nom][g]
    else
        @emvariable c c[:p_min_pu][g,t] * c[:p_nom][g] <= p[g=G,t=T] <= c[:p_max_pu][g,t] * c[:p_nom][g]
    end
end

cost(c::StorageUnit) = sum(c[:marginal_cost] .* c[:p_dispatch]) + sum(c[:capital_cost] .* c[:p_nom])
nodal_balance(c::StorageUnit) = c[:p_dispatch] .- c[:p_store]
function build(m::EnergyModel, c::StorageUnit)
    T = axis(m, :snapshots)
    S = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][s] <= p_nom[s=S] <= c[:p_nom_max][s]

        @emvariable c p_dispatch[s=S,t=T] >= 0
        @emconstraint(c, p_dispatch_upper[s=S,t=T],
                      c[:p_dispatch][s,t] <= c[:p_max_pu][s,t] * c[:p_nom][s])

        @emvariable c p_store[s=S,t=T] >= 0
        @emconstraint(c, p_store_upper[s=S,t=T],
                      c[:p_store][s,t] <= - c[:p_min_pu][s,t] * c[:p_nom][s])

        @emvariable c state_of_charge[s=S,t=T] >= 0
        @emconstraint(c, state_of_charge_upper[s=S,t=T],
                      c[:state_of_charge][s,t] <= c[:max_hours][s,t] * c[:p_nom][s])
    else
        @emvariable c 0 <= p_dispatch[s=S,t=T] <= c[:p_max_pu][s,t] * c[:p_nom][s]
        @emvariable c 0 <= p_store[s=S, t=T] <= - c[:p_min_pu][s,t] * c[:p_nom][s]
        @emvariable c 0 <= state_of_charge[s=S,t=T] <= c[:max_hours][s,t] * c[:p_nom][s]
    end

    @emvariable c 0 <= spill[s=S,t=T] <= c[:inflow][s=S,t=T]

    soc_prev = roll(c[:state_of_charge], :storageunits=>1)
    if !c[:cyclic_state_of_charge] soc_prev[:,1] = c[:state_of_charge_initial] end

    @emconstraint(c, [s=S, t=T],
                  c[:state_of_charge][s,t] - soc_prev[s,t]
                  == c[:p_store][s,t] * c[:efficiency_store]
                  - c[:p_dispatch][s,t] / c[:efficiency_dispatch]
                  + c[:inflow][s,t] - c[:spill][s,t])
end

cost(c::Store) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:e_nom])
nodal_balance(c::Store) = c[:p]
function build(m::EnergyModel, c::Store)
    T = axis(m, :snapshots)
    S = axis(c)

    if isvar(c, :e_nom)
        @emvariable c c[:e_nom_min][s] <= e_nom[s=S] <= c[:e_nom_max][s]

        @emvariable c e[s=S,t=T]
        @emconstraint(c, e_upper[s=S,t=T], c[:e][s,t] <= c[:e_max_pu][s,t] * c[:e_nom][s])
        @emconstraint(c, e_lower[s=S,t=T], c[:e][s,t] >= c[:e_min_pu][s,t] * c[:e_nom][s])
    else
        @emvariable c 0 <= e_dispatch[s=S,t=T] <= c[:e_max_pu][s,t] * c[:e_nom][s]
        @emvariable c 0 <= e_store[s=S, t=T] <= - c[:e_min_pu][s,t] * c[:e_nom][s]
    end

    @emvariable c p[s=S,t=T]

    e_prev = rol(c[:e], :stores=>1)
    if !c[:e_cyclic] e_prev[:,S[1]] = c[:e_initial] end

    @emconstraint(c, [s=S, t=T],
                  c[:e][s,t] - (1. - c[:standing_loss]) * e_prev[s,t] == c[:p]
                  == c[:e_store][s,t] * c[:efficiency_store]
                  - c[:e_dispatch][s,t] / c[:efficiency_dispatch])
end


cost(c::Load) = 0.
nodal_balance(c::Load) = - c[:p_set]
build(c::Load) = nothing

cost(c::Link) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])
function build(m::EnergyModel, c::Link)
    T = axis(m, :snapshots)
    L = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][l] <= p_nom[l=L] <= c[:p_nom_max][l]
        @emvariable c p[l=L,t=T]
        @emconstraint c p_upper[l=L,t=T] p[l,t] <= c[:p_max_pu][l,t] * c[:p_nom][l]
        @emconstraint c p_lower[l=L,t=T] p[l,t] >= - c[:p_min_pu][l,t] * c[:p_nom][l]
    else
        @emvariable c c[:p_min_pu][l,t] * c[:p_nom][l] <= p[l=L,t=T] <= c[:p_max_pu][l,t] * c[:p_nom][l]
    end
end
