## Store
@adddevice(Store, OnePort, :stores, (:S, :T=>:snapshots), joinpath(@__DIR__, "attrs", "stores.csv"))

cost(d::Store) = sum(d[:marginal_cost] .* d[:p]) + sum(d[:capital_cost] .* (d[:e_nom] - getparam(d, :e_nom)))

function addto!(jm::ModelView, m::EnergyModel, d::Store{DF}) where
      {DDF, DF <: LinearExpansionForm{DDF}}

    addto!(jm, m, with_formulation(d, LinearExpansionInvestmentForm))
    addto!(jm, m, with_formulation(d, LinearExpansionDispatchForm{DDF}))
end

function addto!(jm::ModelView, m::EnergyModel, d::Store{DF}) where
      {DF <: LinearExpansionInvestmentForm}

    S = axis(m, d)

    e_nom_min = get(d, :e_nom_min, S)
    e_nom_max = get(d, :e_nom_max, S)

    @variable(jm, e_nom_min[s] <= e_nom[s=S] <= e_nom_max[s])
end

function addto!(jm::ModelView, m::EnergyModel, d::Store{DF}) where
      {DDF, DF <: LinearExpansionDispatchForm{DDF}}

    S = axis(m, d)
    T = axis(m, :snapshots)

    e_nom = get(d, :e_nom, S)
    e_min_pu = get(d, :e_min_pu, S, T)
    e_max_pu = get(d, :e_max_pu, S, T)

    @variable(jm, e[s=S,t=T])

    @constraints jm begin
        e_lower[s=S,t=T], e[s,t] >= e_min_pu[s,t] * e_nom[s]
        e_upper[s=S,t=T], e[s,t] <= e_max_pu[s,t] * e_nom[s]
    end

    addto!_soc(jm, m, d)
end


function addto!(jm::ModelView, m::EnergyModel, d::Store{DF}) where
      {DF <: DispatchForm}

    S = axis(m, d)
    T = axis(m, :snapshots)

    e_nom = get(d, :e_nom, S)
    e_min_pu = get(d, :e_min_pu, S, T)
    e_max_pu = get(d, :e_max_pu, S, T)

    @variable(jm, e_min_pu[s,t] * e_nom[s] <= e[s=S,t=T] <= e_max_pu[s,t] * e_nom[s])

    addto!_soc(jm, m, d)
end


function addto!_soc(jm::ModelView, m::EnergyModel, d::Store)

    S = axis(m, d)
    T = axis(m, :snapshots)

    e = get(d, :e)
    standing_loss = get(d, :standing_loss, S)

    @variable(jm, p[S,T])

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
