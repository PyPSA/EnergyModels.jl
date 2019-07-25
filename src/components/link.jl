## Link

@adddevice(Link, ActiveBranch, LinearExpansionForm{LinearDispatchForm},
           :links, (:L, :T=>:snapshots), joinpath(@__DIR__, "attrs", "links.csv"))

function addtoexpr!(injection::AxisArray, ::NodalActivePower,
                    m::EnergyModel, d::Link)
    B, T = AxisArrays.axes(injection)
    L = axis(m, d)

    bus0 = get(d, :bus0, L)
    bus1 = get(d, :bus1, L)
    p = get(d, :p, L, T)
    eff = get(d, :efficiency, L, T)

    for (i, idx0, idx1) in zip(1:length(L), indexin(bus0, B), indexin(bus1, B)), t in T
        !isnothing(idx0) && add_to_expression!(injection[idx0,t], -1, p[i,t])
        !isnothing(idx1) && add_to_expression!(injection[idx1,t], eff[i,t], p[i,t])
    end
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
