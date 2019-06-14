## Link

@adddevice(Link, ActiveBranch, :links, (:L, :T=>:snapshots), joinpath(@__DIR__, "attrs", "links.csv"))

function p(d::Link)
    p = AxisArray(d[:p])
    eff = d[:efficiency]
    ((l,t)->-p[l,t],       # :bus0
     (l,t)->eff[l]*p[l,t]) # :bus1
end

cost(d::Link) = sum(d[:marginal_cost] .* AxisArray(d[:p])) + sum(d[:capital_cost] .* (AxisArray(d[:p_nom]) - getparam(d, :p_nom)))
function addto!(jm::ModelView, m::EnergyModel, d::Link)
    T = axis(m, :snapshots)
    L = axis(d)

    p_min_pu = get(d, :p_min_pu, L, T)
    p_max_pu = get(d, :p_max_pu, L, T)

    if isvar(d, :p_nom)
        p_nom_min = get(d, :p_nom_min, L)
        p_nom_max = get(d, :p_nom_max, L)

        @variables jm begin
            p_nom_min[l] <= p_nom[l=L] <= p_nom_max[l]
            p[l=L,t=T]
        end

        @constraints jm begin
            p_lower[l=L,t=T], p[l,t] >= p_min_pu[l,t] * p_nom[l]
            p_upper[l=L,t=T], p[l,t] <= p_max_pu[l,t] * p_nom[l]
        end
    else
        p_nom = get(d, :p_nom, L)
        @variable(jm, p_min_pu[l,t] * p_nom[l] <= p[l=L,t=T] <= p_max_pu[l,t] * p_nom[l])
    end
end

