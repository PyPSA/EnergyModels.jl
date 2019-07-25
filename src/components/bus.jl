function addto!(jm::ModelView, m::EnergyModel, bus::Bus)
    T = axis(m, :snapshots)
    B = axis(m, bus)

    injection = expression(NodalActivePower(), m, bus)
    @constraint(jm, p_balance[b=B,t=T], injection[b,t] == 0)
end

addcomponent(Bus, :buses, (:B, :T=>:snapshots), joinpath(@__DIR__, "attrs", "buses.csv"))
