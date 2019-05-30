# Allow one and two-argument forms
build(::EnergyModel, e::ModelElement) = build(e)
cost(::EnergyModel, e::ModelElement) = cost(e)
build(::ModelElement) = error("Not implemented")
cost(::ModelElement) = error("Not implemented")

expression(c::ModelElement, ::ExpressionType) = error("Not implemented")
expression(c::Component, ::Cost) = cost(c)

function build(m::EnergyModel)
    @info("* Equations for individual components")
    for c = components(m)
        @info("  - $(naming(c)) ($(naming(typeof(c))))")
        build(m, c)
    end
    @info("* Equations for energy balance")
    for c = buses(m)
        build(m, c)
    end
    @info("* Equations for subnetworks")
    for sn = subnetworks(m)
        build(m, sn)
    end
    @info("* Cost minimization objective")
    @objective(m.jump, Min, sum(cost(c) for c = components(m)))

    jumpmodel(m) # jupyterlab breaks when trying to display a huge model
end


function build(m::EnergyModel, bus::Bus)
    T = axis(m, :snapshots)
    B = axis(bus)

    terms = []
    for c = components(m), (buses, func) = nodalbalance(c)
        push!(terms, (Dict(b=>findall(isequal(b), buses) for b=B), func))
    end

    @emconstraint(bus, balance[b=B,t=T], sum(f(l,t) for (idx, f)=terms, l=idx[b]) == 0)
end

addelement(Bus{EnergyModel}, :buses, (:B, :T=>:snapshots), joinpath(@__DIR__, "components", "buses.csv"))

function build(m::EnergyModel, sn::SubNetwork)
    T = axis(sn.model, :snapshots)
    branches = sn[PassiveBranch]
    B = axis(branches)
    length(B) == 0 && return

    p = view(branches, :p, (B, T))
    effimp = mapcat(effectiveimpedance, branches)

    C = cycle_matrix(sn)
    Cl = rowvals(C)
    Cv = nonzeros(C)

    @emconstraint(sn, cycles[c=1:size(C,2),t=T], sum(Cv[j] * effimp[Cl[j]] * p[Cl[j],t] for j=nzrange(C,c)) == 0)
end

addelement(SubNetwork{EnergyModel}, :subnetworks)
