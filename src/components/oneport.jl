# OnePort
"Connected to exactly one `Bus`, determined by :bus attribute"
abstract type OnePort <: Component end
for component = (:Generator, :Load, :StorageUnit, :Store)
    @eval begin
        struct $component <: OnePort
            model::EnergyModel
            class::Symbol
        end
    end
end

busattributes(c::OnePort) = (:bus,)

## Defaults for OnePort
cost(c::OnePort) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])
nodalbalance(c::OnePort) = (p = c[:p]; (c[:bus] => (o,t)->p[o,t],))

## Generator
function build(c::Generator)
    T = axis(c, :snapshots)
    G = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][g] <= p_nom[g=G] <= c[:p_nom_max][g]
    end

    @emvariable c c[:p_min_pu][g,t] * c[:p_nom][g] <= p[g=G,t=T] <= c[:p_max_pu][g,t] * c[:p_nom][g]
end

addelement(Generator, :generators, (:G, :T=>:snapshots), joinpath(@__DIR__, "generators.csv"))

## StorageUnit
cost(c::StorageUnit) = sum(c[:marginal_cost] .* c[:p_dispatch]) + sum(c[:capital_cost] .* c[:p_nom])
function nodalbalance(c::StorageUnit)
    p_dispatch = c[:p_dispatch]
    p_store = c[:p_store]

    (c[:bus] => (s,t)->p_dispatch[s,t] - p_store[s,t],)
end

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

    soc = c[:state_of_charge]
    if c[:cyclic_state_of_charge]
        soc_prev = circshift(soc, :snapshots=>1)
    else
        soc_prev = similar(soc, Union{Float64,eltype(soc)})
        soc_prev[:,1] .= c[:state_of_charge_initial]
        soc_prev[:,2:end] .= soc[:,1:end-1]
    end

    @emconstraint(c, soc_eq[s=S, t=T],
                  soc[s,t] - soc_prev[s,t]
                  == c[:p_store][s,t] * c[:efficiency_store][s]
                  - c[:p_dispatch][s,t] / c[:efficiency_dispatch][s]
                  + c[:inflow][s,t] - c[:spill][s,t])
end

addelement(StorageUnit, :storageunits, (:S, :T=>:snapshots), joinpath(@__DIR__, "storageunits.csv"))

# add_component(StorageUnit, "storageunits", Axes(:S, :T=>:snapshots), joinpath(@__DIR__, "storageunits.csv"))
# VarParam(:p_nom, 0, :S), Param(:p_nom_min, 0, :S), Param(:p_nom_max, Inf, :S),
# Variable(:p, 0, :S, :T), Param(:p_min_pu, -1, :S, :T), Param(:p_max_pu, 1, :S, :T),
# Param(:efficiency_store, 1, :S, :T), Param(:efficiency_dispatch, 1, :S, :T),
# Param(:standing_loss, 0, :S, :T), Param(:efficiency_dispatch, 1, :S, :T),



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
                  c[:e][s,t] - (1 - c[:standing_loss][s]) * e_prev[s,t] == c[:p])
end

addelement(Store, :stores, (:S, :T=>:snapshots), joinpath(@__DIR__, "stores.csv"))

## Load
cost(c::Load) = 0.
nodalbalance(c::Load) = (p = c[:p_set]; (c[:bus] => (l,t)->-p[l,t],))
build(c::Load) = nothing

addelement(Load, :loads, (:L,), joinpath(@__DIR__, "loads.csv"))
