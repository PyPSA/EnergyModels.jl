function addto!(jm::ModelView, m::EnergyModel, bus::Bus)
    injection = expression(NodalActivePower(), m, bus)
    B, T = AxisArrays.axes(injection)
    @constraint(jm, p_balance[b=B,t=T], injection[b,t] == 0)
end

addcomponent(Bus, :buses, (:B, :T=>:snapshots), joinpath(@__DIR__, "attrs", "buses.csv"))
