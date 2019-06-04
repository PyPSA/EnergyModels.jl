"""
    addto!(jm::JuMP.AbstractModel, m::EnergyModel)
    addto!(jm::JuMP.AbstractModel, m::EnergyModel, c::Element)

Add the variables and constraints defining the energy model `m` to the jump
model `jm` and set the objective function. If an element `c` is specified, the
element is added alone.
"""
function addto! end

# Allow one and two-argument forms
build(::EnergyModel, e::Element) = build(e)
cost(::EnergyModel, e::Element) = cost(e)
build(::ModelElement) = error("Not implemented")
cost(::ModelElement) = error("Not implemented")

expression(c::ModelElement, ::ExpressionType) = error("Not implemented")
expression(c::Component, ::Cost) = cost(c)

function addto!(jm::JuMP.AbstractModel, m::EnergyModel)
    @info("* Equations for individual components")
    for c = components(m)
        @info("  - $(naming(c)) ($(naming(typeof(c))))")
        addto!(jm, m, c)
    end
    @info("* Equations for energy balance")
    for c = buses(m)
        addto!(jm, m, c)
    end
    @info("* Equations for subnetworks")
    for sn = subnetworks(m)
        addto!(jm, m, sn)
    end
    @info("* Cost minimization objective")
    @objective(jm, Min, sum(cost(c) for c = components(m)))

    jm # TODO? jupyterlab breaks when trying to display a huge model
end

# Wraps the AbstractModel in a ModelView to store variables and constraints in c.objects
addto!(jm::JuMP.AbstractModel, m::EnergyModel, c::Element) = addto!(ModelView(jm, c), m, c)

function addto!(jm::ModelView, m::EnergyModel, bus::Bus)
    T = axis(m, :snapshots)
    B = axis(bus)

    # TODO One could also rearrange this addto function by finding the components on each bus first
    terms = []
    for c in components(m), (busattr, p_func) in zip(busattributes(c), p(c))
        buses = get(c, busattr)
        push!(terms, (Dict(b=>findall(isequal(b), buses) for b in B), p_func))
    end

    @constraint(jm, p_balance[b=B,t=T], sum(f(l,t) for (idx, f) in terms, l in idx[b]) == 0)
end

addelement(Bus{EnergyModel}, :buses, (:B, :T=>:snapshots), joinpath(@__DIR__, "components", "buses.csv"))

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

addelement(SubNetwork{EnergyModel}, :subnetworks)
