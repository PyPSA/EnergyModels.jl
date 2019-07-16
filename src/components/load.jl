## Load
@adddevice(Load, OnePort, :loads, (:L,), joinpath(@__DIR__, "attrs", "loads.csv"))

cost(d::Load) = 0.
function p(d::Load)
    p = get(d, :p_set, axis(d), axis(d, :snapshots))
    ((l,t)->-p[l,t],)
end
addto!(jm::JuMP.AbstractModel, m::EnergyModel, d::Load) = nothing
