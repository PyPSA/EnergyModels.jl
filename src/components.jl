EnergyModel(filename::String; kwargs...) = EnergyModel(load(filename); kwargs...)
EnergyModel(data::Data; solver=GurobiSolver()) = load(EnergyModel(Dict{Symbol,Component}(), data, Model(solver=solver)))

function load(m::EnergyModel)
    for T = components(m.data), class = classes(m.data, T) push!(m, T(m, class)) end
    m
end

Base.push!(m::EnergyModel, c::Component) = (m.components[naming(Symbol, c, c.class)] = c)

components(m::EnergyModel) = values(m.components)
components(m::EnergyModel, T::Type{<:Component}) = (c for c = values(m.components) if isa(c, T))

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

# Allow one and two-argument forms
cost(::EnergyModel, c::Component) = cost(c)
build(::EnergyModel, c::Component) = build(c)

function Base.getindex(c::Component, attr::Symbol)
    name = naming(Symbol, c, c.class, attr)
    m = jumpmodel(c)
    haskey(m.objDict, name) ? m.objDict[name] : get(c.model.data, c, c.class, attr)
end

@inline function add_nodal_balance!(balance, bus, expressions)
    components, = axisvalues(bus)
    for i = eachindex(bus)
        balance[bus[i],:] .+= expressions[components[i],:]
    end
    balance
end

nodal_balance(m::EnergyModel) = nodal_balance(m, axis(m, :buses))
function nodal_balance(m::EnergyModel, buses::Axis)
    T = axis(m, :snapshots)
    balance = AxisArray(zeros(AffExpr, length.((buses, T))...), buses, T)
    for c = m.components add_nodal_balance!(balance, c) end
    balance
end

# TODO probably introduce an indirection so that the array is only wrapped if extra axes are needed
view(c::Component, attr::Symbol, axes) = WrappedArray(c[attr], axes...)

function axis(m::EnergyModel, attr::Symbol)
    if attr == :buses
        buses = collect(components(m, Bus))
        @assert length(buses) == 1
        axis(first(buses))
    else
        axis(m.data, attr)
    end
end

function axis(sn::SubNetwork, attr::Symbol; ctype=PassiveBranchComponent)
    if attr == :buses
        Axis{:buses}(axis(sn.model, :buses)[sn.buses])
    elseif attr == :branches
        buses = axis(sn, :buses)
        ax = Symbol[]
        for c = components(sn.model, ctype)
            inds = intersect(findin(c[:bus0], buses.val), findin(c[:bus1], buses.val))
            append!(ax, naming.(Symbol, c, c.class, axis(c)[inds]))
        end
        Axis{:branches}(ax)
    else
        axis(sn.model, attr)
    end
end


axis(m::EnergyModel, args...) = axis(m.data, args...)
axis(c::Component) = axis(c.model, c, c.class)
axis(c::Component, attr) = axis(c.model, attr)

# Could be specialized to not have to retrieve the whole axis (on the other
# hand, the axis should be cached, anyway)
Base.length(c::Component) = length(axis(c))

## Defaults for OnePortComponents
cost(c::OnePortComponent) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])
add_nodal_balance!(balance, c::OnePortComponent) = add_nodal_balance!(balance, c[:bus], nodal_balance(c))
nodal_balance(c::OnePortComponent) = c[:p]

## Generator
function build(c::Generator)
    T = axis(c, :snapshots)
    G = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][g] <= p_nom[g=G] <= c[:p_nom_max][g]
    end

    @emvariable c c[:p_min_pu][g,t] * c[:p_nom][g] <= p[g=G,t=T] <= c[:p_max_pu][g,t] * c[:p_nom][g]
end

## StorageUnit
cost(c::StorageUnit) = sum(c[:marginal_cost] .* c[:p_dispatch]) + sum(c[:capital_cost] .* c[:p_nom])
nodal_balance(c::StorageUnit) = c[:p_dispatch] .- c[:p_store]
function build(c::StorageUnit)
    T = axis(c, :snapshots)
    S = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][s] <= p_nom[s=S] <= c[:p_nom_max][s]
    end

    @emvariable c 0 <= p_dispatch[s=S,t=T] <= c[:p_max_pu][s,t] * c[:p_nom][s]
    @emvariable c 0 <= p_store[s=S, t=T] <= - c[:p_min_pu][s,t] * c[:p_nom][s]
    @emvariable c 0 <= state_of_charge[s=S,t=T] <= c[:max_hours][s,t] * c[:p_nom][s]
    @emvariable c 0 <= spill[s=S,t=T] <= c[:inflow][s,t]

    soc_prev = circshift(c[:state_of_charge], :snapshots=>1)
    if !c[:cyclic_state_of_charge] soc_prev[:,T[1]] .= c[:state_of_charge_initial] end

    @emconstraint(c, soc_eq[s=S, t=T],
                  c[:state_of_charge][s,t] - soc_prev[s,t]
                  == c[:p_store][s,t] * c[:efficiency_store]
                  - c[:p_dispatch][s,t] / c[:efficiency_dispatch]
                  + c[:inflow][s,t] - c[:spill][s,t])
end

## Store
cost(c::Store) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:e_nom])
function build(c::Store)
    T = axis(c, :snapshots)
    S = axis(c)

    if isvar(c, :e_nom)
        @emvariable c c[:e_nom_min][s] <= e_nom[s=S] <= c[:e_nom_max][s]
    end

    @emvariable c c[:e_min_pu][s,t] * c[:e_nom][s] <= e[s=S,t=T] <= c[:e_max_pu][s,t] * c[:e_nom][s]
    @emvariable c p[s=S,t=T]

    e_prev = circshift(c[:e], :snapshots=>1)
    if !c[:e_cyclic] e_prev[:,T[1]] .= c[:e_initial] end

    @emconstraint(c, e_eq[s=S, t=T],
                  c[:e][s,t] - (1. - c[:standing_loss][s]) * e_prev[s,t] == c[:p])
end

## Load
cost(c::Load) = 0.
nodal_balance(c::Load) = - c[:p_set]
build(c::Load) = nothing


## Default for BranchComponents
function add_nodal_balance!(balance, c::BranchComponent)
    add_nodal_balance!(balance, c[:bus0], -c[:p])
    add_nodal_balance!(balance, c[:bus1], c[:p])
end

## Link
function add_nodal_balance!(balance, c::Link)
    add_nodal_balance!(balance, c[:bus0], -c[:p])
    add_nodal_balance!(balance, c[:bus1], c[:efficiency].*c[:p])
end
cost(c::Link) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])
function build(c::Link)
    L = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][l] <= p_nom[l=L] <= c[:p_nom_max][l]
    end

    @emvariable c c[:p_min_pu][l,t] * c[:p_nom][l] <= p[l=L,t=:snapshots] <= c[:p_max_pu][l,t] * c[:p_nom][l]
end

## Line
cost(c::Line) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:s_nom])
function add_nodal_balance!(balance, c::Line)
    add_nodal_balance!(balance, c[:bus0], c[:p])
    add_nodal_balance!(balance, c[:bus1], -c[:p])
end
function build(c::Line)
    L = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:s_nom_min][l] <= s_nom[l=L] <= c[:s_nom_max][l]
    end

    @emvariable c -c[:s_max_pu][l,t] * c[:s_nom][l] <= p[l=L,t=:snapshots] <= c[:s_max_pu][l,t] * c[:s_nom][l]
end

