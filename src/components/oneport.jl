# OnePort
"Connected to exactly one `Bus`, determined by :bus attribute"
abstract type OnePort <: Component end
for component = (:Generator, :Load, :StorageUnit, :Store)
    @eval begin
        struct $component <: OnePort
            model::EnergyModel
            class::Symbol
            objects::Dict{Symbol,Any}
        end
    end
end

busattributes(c::OnePort) = (:bus,)

## Defaults for OnePort
cost(c::OnePort) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* (c[:p_nom] - getparam(c, :p_nom)))
function p(c::OnePort)
    p = c[:p]
    (o,t)->p[o,t],
end

## Generator
function addto!(jm::ModelView, m::EnergyModel, c::Generator)
    T = axis(c, :snapshots)
    G = axis(c)

    p_min_pu = get(c, :p_min_pu, G, T)
    p_max_pu = get(c, :p_max_pu, G, T)

    if isvar(c, :p_nom)
        p_nom_min = get(c, :p_nom_min, G)
        p_nom_max = get(c, :p_nom_max, G)
        @variables jm begin
            p_nom_min[g] <= p_nom[g=G] <= p_nom_max[g]
            p[g=G,t=T]
        end

        @constraints jm begin
            p_lower[g=G,t=T], p[g,t] >= p_min_pu[g,t] * p_nom[g]
            p_upper[g=G,t=T], p[g,t] <= p_max_pu[g,t] * p_nom[g]
        end
    else
        p_nom = get(c, :p_nom, G)
        @variable(jm, p_min_pu[g,t] * p_nom[g] <= p[g=G,t=T] <= p_max_pu[g,t] * p_nom[g])
    end
end

addelement(Generator, :generators, (:G, :T=>:snapshots), joinpath(@__DIR__, "generators.csv"))

## StorageUnit
cost(c::StorageUnit) = sum(c[:marginal_cost] .* c[:p_dispatch]) + sum(c[:capital_cost] .* (c[:p_nom] - getparam(c, :p_nom)))
function p(c::StorageUnit)
    p_dispatch = c[:p_dispatch]
    p_store = c[:p_store]

    (s,t)->p_dispatch[s,t] - p_store[s,t],
end

function addto!(jm::ModelView, m::EnergyModel, c::StorageUnit)
    T = axis(c, :snapshots)
    S = axis(c)

    p_min_pu = get(c, :p_min_pu, S, T)
    p_max_pu = get(c, :p_max_pu, S, T)

    if isvar(c, :p_nom)
        p_nom_min = get(c, :p_nom_min, S)
        p_nom_max = get(c, :p_nom_max, S)

        @variables jm begin
            p_nom_min[s] <= p_nom[s=S] <= p_nom_max[s]
            p_store[s=S,t=T] >= 0
            p_dispatch[s=S,t=T] >= 0
        end

        @constraints jm begin
            p_lower[s=S,t=T], p_store[s,t] <= - p_min_pu[s,t] * p_nom[s]
            p_upper[s=S,t=T], p_dispatch[s,t] <= p_max_pu[s,t] * p_nom[s]
        end
    else
        p_nom = get(c, :p_nom, S)

        @variables jm begin
            0 <= p_store[s=S,t=T] <= - p_min_pu[s,t] * p_nom[s]
            0 <= p_dispatch[s=S,t=T] <= p_max_pu[s,t] * p_nom[s]
        end
    end

    inflow = get(c, :inflow, S, T)
    max_hours = get(c, :max_hours, S, T)
    efficiency_store = get(c, :efficiency_store, S, T)
    efficiency_dispatch = get(c, :efficiency_dispatch, S, T)

    @variables jm begin
        0 <= spill[s=S,t=T] <= inflow[s,t]
        0 <= state_of_charge[s=S,t=T]
    end

    @constraint(jm, state_of_charge_upper[s=S,t=T], state_of_charge[s,t] <= max_hours[s,t] * p_nom[s])

    if c[:cyclic_state_of_charge]
        soc_prev = circshift(state_of_charge, :snapshots=>1)
    else
        soc_prev = similar(state_of_charge, Union{Float64,eltype(soc)})
        soc_prev[:,1] .= get(c, :state_of_charge_initial, S)
        soc_prev[:,2:end] .= state_of_charge[:,1:end-1]
    end

    @constraint(jm, state_of_charge_eq[s=S, t=T],
                state_of_charge[s,t] - soc_prev[s,t]
                == p_store[s,t] * efficiency_store[s,t]
                - p_dispatch[s,t] / efficiency_dispatch[s,t]
                + inflow[s,t] - spill[s,t])
end

addelement(StorageUnit, :storageunits, (:S, :T=>:snapshots), joinpath(@__DIR__, "storageunits.csv"))

## Store
cost(c::Store) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* (c[:e_nom] - getparam(c, :e_nom)))
function addto!(jm::ModelView, m::EnergyModel, c::Store)
    T = axis(c, :snapshots)
    S = axis(c)

    e_min_pu = get(c, :e_min_pu, S, T)
    e_max_pu = get(c, :e_max_pu, S, T)

    if isvar(c, :e_nom)
        e_nom_min = get(c, :e_nom_min, S)
        e_nom_max = get(c, :e_nom_max, S)

        @variables jm begin
            e_nom_min[s] <= e_nom[s=S] <= e_nom_max[s]
            e[s=S,t=T]
        end

        @constraints jm begin
            e_lower[s=S,t=T], e[s,t] >= e_min_pu[s,t] * e_nom[s]
            e_upper[s=S,t=T], e[s,t] <= e_max_pu[s,t] * e_nom[s]
        end
    else
        e_nom = get(c, :e_nom, S)

        @variables jm begin
            e_min_pu[s,t] * e_nom[s] <= e[s=S,t=T] <= e_max_pu[s,t] * e_nom[s]
        end
    end

    @variable(jm, p[s=S,t=T])

    standing_loss = get(c, :standing_loss, S)

    if c[:e_cyclic]
        e_prev = circshift(e, :snapshots=>1)
    else
        e_prev = similar(e, Union{Float64,eltype(e)})
        e_prev[:,1] .= c[:e_initial]
        e_prev[:,2:end] .= e[:,1:end-1]
    end
    @constraint(jm, e_eq[s=S, t=T],
                e[s,t] - (1 - standing_loss[s]) * e_prev[s,t] == p)
end

addelement(Store, :stores, (:S, :T=>:snapshots), joinpath(@__DIR__, "stores.csv"))

## Load
cost(c::Load) = 0.
function p(c::Load)
    p = c[:p_set]
    (l,t)->-p[l,t],
end
addto!(jm::JuMP.AbstractModel, m::EnergyModel, c::Load) = nothing

addelement(Load, :loads, (:L,), joinpath(@__DIR__, "loads.csv"))
