## Link

@adddevice(Link, ActiveBranch, :links, (:L, :T=>:snapshots), joinpath(@__DIR__, "attrs", "links.csv"))

function p(d::Link)
    p = d[:p]
    eff = d[:efficiency]
    ((l,t)->-p[l,t],       # :bus0
     (l,t)->eff[l]*p[l,t]) # :bus1
end

cost(d::Link) = sum(d[:marginal_cost] .* d[:p]) + sum(d[:capital_cost] .* (d[:p_nom] - getparam(d, :p_nom)))

function addto!(jm::ModelView, m::EnergyModel, d::Link{DF}) where {DDF, DF <: LinearExpansionForm{DDF}}
    addto!(jm, m, with_formulation(d, LinearExpansionInvestmentForm))
    addto!(jm, m, with_formulation(d, LinearExpansionDispatchForm{DDF}))
end

function addto!(jm::ModelView, m::EnergyModel, d::Link{DF}) where DF <: LinearExpansionInvestmentForm
    L = axis(d)

    p_nom_min = get(d, :p_nom_min, L)
    p_nom_max = get(d, :p_nom_max, L)

    @variable(jm, p_nom_min[l] <= p_nom[l=L] <= p_nom_max[l])
end

function addto!(jm::ModelView, m::EnergyModel, d::Link{DF}) where
      {DDF, DF <: LinearExpansionDispatchForm{DDF}}

    L = axis(m, d)
    T = axis(m, :snapshots)

    p_nom = get(d, :p_nom)
    p_min_pu = get(d, :p_min_pu, L, T)
    p_max_pu = get(d, :p_max_pu, L, T)

    @variable(jm, p[l=L,t=T])

    @constraints jm begin
        p_lower[l=L,t=T], p[l,t] >= p_min_pu[l,t] * p_nom[l]
        p_upper[l=L,t=T], p[l,t] <= p_max_pu[l,t] * p_nom[l]
    end
end

function addto!(jm::ModelView, m::EnergyModel, d::Link{DF}) where
      {DF <: DispatchForm}

    L = axis(m, d)
    T = axis(m, :snapshots)

    p_nom = get(d, :p_nom)
    p_min_pu = get(d, :p_min_pu, L, T)
    p_max_pu = get(d, :p_max_pu, L, T)

    @variable(jm, p_min_pu[l,t] * p_nom[l] <= p[l=L,t=T] <= p_max_pu[l,t] * p_nom[l])
end
