## Generator
@adddevice(Generator, OnePort, LinearExpansionForm{LinearDispatchForm},
           :generators, (:G, :T=>:snapshots), joinpath(@__DIR__, "attrs", "generators.csv"))

function addto!(jm::ModelView, m::EnergyModel, d::Generator{DF}) where {DDF, DF <: LinearExpansionForm{DDF}}
    addto!(jm, m, with_formulation(d, LinearExpansionInvestmentForm))
    addto!(jm, m, with_formulation(d, LinearExpansionDispatchForm{DDF}))
end

function addto!(jm::ModelView, m::EnergyModel{MT,TF}, d::Generator{DF}) where
        {MT <: ModelType, TF <: PM.AbstractActivePowerFormulation, DF <: LinearExpansionInvestmentForm}
    G = axis(m, d)

    p_nom_min = get(d, :p_nom_min, G)
    p_nom_max = get(d, :p_nom_max, G)

    @variable(jm, p_nom_min[g] <= p_nom[g=G] <= p_nom_max[g])
end

function addto!(jm::ModelView, m::EnergyModel{MT,TF}, d::Generator{DF}) where
        {MT <: ModelType, TF <: PM.AbstractActivePowerFormulation, DDF, DF <: LinearExpansionDispatchForm{DDF}}
    G = axis(m, d)
    T = axis(m, :snapshots)

    p_nom = get(d, :p_nom, G)
    p_min_pu = get(d, :p_min_pu, G, T)
    p_max_pu = get(d, :p_max_pu, G, T)

    @variable(jm, p[G,T])

    @constraints jm begin
        p_lower[g=G,t=T], p[g,t] >= p_min_pu[g,t] * p_nom[g]
        p_upper[g=G,t=T], p[g,t] <= p_max_pu[g,t] * p_nom[g]
    end

    if DDF <: UnitCommittmentForm
        # Add variables and constraints for UC
    end
end

function addto!(jm::ModelView, m::EnergyModel{MT,TF}, d::Generator{DF}) where
        {MT <: ModelType, TF <: PM.AbstractActivePowerFormulation, DF <: DispatchForm}
    T = axis(m, :snapshots)
    G = axis(m, d)

    p_min_pu = get(d, :p_min_pu, G, T)
    p_max_pu = get(d, :p_max_pu, G, T)

    p_nom = get(d, :p_nom, G)
    @variable(jm, p_min_pu[g,t] * p_nom[g] <= p[g=G,t=T] <= p_max_pu[g,t] * p_nom[g])

    if DF <: UnitCommittmentForm
        # Add variables and constraints for UC

        # Maybe share a function with the expansion form above as well as generators and other oneports
    end
end
