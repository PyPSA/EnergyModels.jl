## Store
@adddevice(Store, OnePort, :stores, (:S, :T=>:snapshots), joinpath(@__DIR__, "attrs", "stores.csv"))

cost(d::Store) = sum(d[:marginal_cost] .* AxisArray(d[:p])) + sum(d[:capital_cost] .* (AxisArray(d[:e_nom]) - getparam(d, :e_nom)))

function addto!(jm::ModelView, m::EnergyModel, d::Store)
    T = axis(d, :snapshots)
    S = axis(d)

    e_min_pu = get(d, :e_min_pu, S, T)
    e_max_pu = get(d, :e_max_pu, S, T)

    if isvar(d, :e_nom)
        e_nom_min = get(d, :e_nom_min, S)
        e_nom_max = get(d, :e_nom_max, S)

        @variables jm begin
            e_nom_min[s] <= e_nom[s=S] <= e_nom_max[s]
            e[s=S,t=T]
        end

        @constraints jm begin
            e_lower[s=S,t=T], e[s,t] >= e_min_pu[s,t] * e_nom[s]
            e_upper[s=S,t=T], e[s,t] <= e_max_pu[s,t] * e_nom[s]
        end
    else
        e_nom = get(d, :e_nom, S)

        @variables jm begin
            e_min_pu[s,t] * e_nom[s] <= e[s=S,t=T] <= e_max_pu[s,t] * e_nom[s]
        end
    end

    @variable(jm, p[s=S,t=T])

    standing_loss = get(d, :standing_loss, S)

    if d[:e_cyclic]
        e_prev = circshift(e, :snapshots=>1)
    else
        e_prev = similar(e, Union{Float64,eltype(e)})
        e_prev[:,1] .= d[:e_initial]
        e_prev[:,2:end] .= e[:,1:end-1]
    end
    @constraint(jm, e_eq[s=S, t=T],
                e[s,t] - (1 - standing_loss[s]) * e_prev[s,t] == p)
end
