## Load
@adddevice(Load, OnePort, LinearDispatchForm, :loads, (:L,), joinpath(@__DIR__, "attrs", "loads.csv"))

cost(d::Load) = 0.
function addtoexpr!(injection::AxisArray, ::NodalActivePower,
                    m::EnergyModel, d::Load)
    B, T = AxisArrays.axes(injection)
    D = axis(m, d)

    buses = get(d, :bus, D)
    p_set = get(d, :p_set, D, T)

    for (i, idx) in enumerate(indexin(buses, B)), t in T
        !isnothing(idx) && add_to_expression!(injection[idx,t], -p_set[i,t])
    end
end
addto!(jm::JuMP.AbstractModel, m::EnergyModel, d::Load) = nothing
