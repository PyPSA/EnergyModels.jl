function addto!(jm::ModelView, m::EnergyModel, bus::Bus)
    T = axis(m, :snapshots)
    B = axis(bus)

    # TODO One could also rearrange this addto function by finding the devices on each bus first
    terms = []
    for d in devices(m), (busattr, p_func) in zip(busattributes(d), p(d))
        buses = get(d, busattr)
        push!(terms, (Dict(b=>findall(isequal(b), buses) for b in B), p_func))
    end

    @constraint(jm, p_balance[b=B,t=T], sum(f(l,t) for (idx, f) in terms, l in idx[b]) == 0)
end

addcomponent(Bus{EnergyModel}, :buses, (:B, :T=>:snapshots), joinpath(@__DIR__, "attrs", "buses.csv"))
