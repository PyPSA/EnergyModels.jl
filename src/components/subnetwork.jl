function addto!(jm::ModelView, m::EnergyModel, sn::SubNetwork)
    T = axis(sn.model, :snapshots)
    branches = sn[PassiveBranch]
    B = axis(branches)
    length(B) == 0 && return

    p = get(branches, :p, B, T)
    effimp = mapcat(effectiveimpedance, branches)

    C = cycle_matrix(sn)
    Cl = rowvals(C)
    Cv = nonzeros(C)

    @constraint(jm, cycles[c=1:size(C,2),t=T], sum(Cv[j] * effimp[Cl[j]] * p[Cl[j],t] for j=nzrange(C,c)) == 0)
end

addcomponent(SubNetwork{EnergyModel}, :subnetworks)
