@adddevice(StorageUnit, OnePort, :storageunits, (:S, :T=>:snapshots), joinpath(@__DIR__, "attrs", "storageunits.csv"))

## StorageUnit
cost(d::StorageUnit) = sum(d[:marginal_cost] .* AxisArray(d[:p_dispatch])) + sum(d[:capital_cost] .* (AxisArray(d[:p_nom]) - getparam(d, :p_nom)))
function p(d::StorageUnit)
    p_dispatch = AxisArray(d[:p_dispatch])
    p_store = AxisArray(d[:p_store])

    ((s,t)->p_dispatch[s,t] - p_store[s,t],)
end

function addto!(jm::ModelView, m::EnergyModel, d::StorageUnit)
    T = axis(d, :snapshots)
    S = axis(d)

    p_min_pu = get(d, :p_min_pu, S, T)
    p_max_pu = get(d, :p_max_pu, S, T)

    if isvar(d, :p_nom)
        p_nom_min = get(d, :p_nom_min, S)
        p_nom_max = get(d, :p_nom_max, S)

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
        p_nom = get(d, :p_nom, S)

        @variables jm begin
            0 <= p_store[s=S,t=T] <= - p_min_pu[s,t] * p_nom[s]
            0 <= p_dispatch[s=S,t=T] <= p_max_pu[s,t] * p_nom[s]
        end
    end

    inflow = get(d, :inflow, S, T)
    max_hours = get(d, :max_hours, S, T)
    efficiency_store = get(d, :efficiency_store, S, T)
    efficiency_dispatch = get(d, :efficiency_dispatch, S, T)

    @variables jm begin
        0 <= spill[s=S,t=T] <= inflow[s,t]
        0 <= state_of_charge[s=S,t=T]
    end

    @constraint(jm, state_of_charge_upper[s=S,t=T], state_of_charge[s,t] <= max_hours[s,t] * p_nom[s])

    if d[:cyclic_state_of_charge]
        soc_prev = circshift(state_of_charge, :snapshots=>1)
    else
        soc_prev = similar(state_of_charge, Union{Float64,eltype(soc)})
        soc_prev[:,1] .= get(d, :state_of_charge_initial, S)
        soc_prev[:,2:end] .= state_of_charge[:,1:end-1]
    end

    @constraint(jm, state_of_charge_eq[s=S, t=T],
                state_of_charge[s,t] - soc_prev[s,t]
                == p_store[s,t] * efficiency_store[s,t]
                - p_dispatch[s,t] / efficiency_dispatch[s,t]
                + inflow[s,t] - spill[s,t])
end
