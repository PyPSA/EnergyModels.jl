EnergyModel(filename::String; kwargs...) = EnergyModel(load(filename); kwargs...)
EnergyModel(data::Data; solver=GurobiSolver()) = load(EnergyModel(Dict{Symbol,Component}(), SubNetwork[], data, Model(solver=solver)))

function load(m::EnergyModel)
    for T = components(m.data), class = classes(m.data, T) push!(m, T(m, class)) end
    determine_subnetworks!(m)
    m
end

Base.push!(m::EnergyModel, c::Component) = (m.components[c.class] = c)

components(m::EnergyModel) = values(m.components)
components(sn::SubNetwork) = components(sn.model)
components(cv::AbstractContainerView) = cv.components
components(m::Container, T::Type{<:Component}) = (c for c = components(m) if isa(c, T))
components(a::Union{SubNetwork,AbstractContainerView}) = a.components

JuMP.solve(m::EnergyModel; kwargs...) = solve(m.jump; kwargs...)

Base.show(io::IO, m::EnergyModel) = print(io, typeof(m), " with ", length(m.components), " components")
Base.show(io::IO, c::Component) = print(io, typeof(c), " for class ", c.class)
# function Base.show(io::IO, ::MIME"text/plain", c::Component)
#     println(io, c, " with ")
#     println(io, "* ", length(c.vars), " variables")
#     print(io, "* ", length(c.constrs), " constraints")
# end

isvar(c::Component, attr::Symbol) = isvar(c.model.data, c, c.class, attr)

build(::Model, ::Component) = error("Not implemented")
build(::Component) = error("Not implemented")

jumpmodel(c::Component) = c.model.jump
jumpmodel(m::EnergyModel) = m.jump

expression(c::Component, ::ExpressionType) = error("Not implemented")
expression(c::Component, ::Cost) = cost(c)
cost(::Component) = error("Not implemented")

# Allow one and two-argument forms
cost(::EnergyModel, c::Component) = cost(c)
build(::EnergyModel, c::Component) = build(c)

function Base.getindex(c::Component, attr::Symbol)
    name = Symbol(c.class, attr)
    m = jumpmodel(c)
    haskey(m.objDict, name) ? AxisArray(m.objDict[name]) : get(c.model.data, c, c.class, attr)
end

Base.getindex(m::EnergyModel, class::Symbol) = m.components[class]
Base.getindex(m::EnergyModel, T::Type{<:Component}) = ContainerView(m, collect(components(m, T)))
Base.getindex(sn::SubNetwork, T::Type{<:Component}) = SubContainerView(sn.model, collect(components(sn, T)), sn.buses)

add_nodal_balance!(balance, buses, expressions::JuMPArray) = add_nodal_balance!(balance, buses, AxisArray(expressions))
function add_nodal_balance!(balance, buses, expressions)
    components, = axisvalues(buses)
    for i = eachindex(buses)
        balance[buses[i],:] .+= expressions[components[i],:]
    end
    balance
end

nodal_balance(m::EnergyModel) = nodal_balance(m, axis(m[Bus]))
function nodal_balance(m::EnergyModel, buses::Axis)
    T = axis(m, :snapshots)
    balance = AxisArray(zeros(AffExpr, length.((buses, T))...), buses, T)
    for c = components(m)
        isa(c, Bus) && continue
        info("Adding ", c.class, " to energy balance")
        add_nodal_balance!(balance, c)
    end
    balance
end

# TODO probably introduce an indirection so that the array is only wrapped if extra axes are needed
Base.view(c::Union{Component, AbstractContainerView}, attr::Symbol, axes) = WrappedArray(c[attr], axes...)

axis(m::EnergyModel, args...) = axis(m.data, args...)
axis(c::Component) = axis(c.model, c, c.class)
axis(c::Component, attr) = axis(c.model, attr)

tupleaxisvals(c::Bus, A) = A.val
tupleaxisvals(c) = tupleaxisvals(c, axis(c))
tupleaxisvals(c, A) = AxisArrays.CategoricalVector(tuple.(c.class, A.val))

indicesinbuses(c::OnePort, buses) = indicesinbuses(c, buses, :bus)
indicesinbuses(c::Branch, buses) = indicesinbuses(c, buses, :bus0, :bus1)
indicesinbuses(c::Bus, buses) = findin(axis(c), buses.val)
indicesinbuses(c::Union{OnePort,Branch}, buses, busattrs...) = intersect((findin(c[attr], buses.val) for attr = busattrs)...)

_subsetifbuses(cv::ContainerView, c, A) = A
_subsetifbuses(cv::SubContainerView, c::Component, A) = A[indicesinbuses(c, cv.buses)]

_viewaxis(cv::AbstractContainerView{T}, a) where T = Axis{Symbol(naming(T))}(a)
axis(cv::AbstractContainerView) = _viewaxis(cv, vcat((_subsetifbuses(cv, c, tupleaxisvals(c)) for c = components(cv))...))

Base.cat(cv::AbstractContainerView, cs::Vector{<:Component}, as::Vector) = @consense(as, "Single values do not agree")
function Base.cat(cv::AbstractContainerView, cs::Vector{<:Component}, as::Vector{<:AxisArray})
    _viewaxisarray(c, a) = AxisArray(a, _viewaxis(cv, tupleaxisvals(c, first(axes(a)))))
    cat(1, (_viewaxisarray(c, _subsetifbuses(cv, c, a)) for (c, a) = zip(cs, as))...)
end

mapcat(f::Function, cv::AbstractContainerView) = cat(cv, components(cv), map(f, components(cv)))

Base.getindex(cv::AbstractContainerView, i) = mapcat(c->c[i], cv)

# Could be specialized to not have to retrieve the whole axis (on the other
# hand, the axis should be cached, anyway)
Base.length(c::Component) = length(axis(c))

## Defaults for OnePort
cost(c::OnePort) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])
add_nodal_balance!(balance, c::OnePort) = add_nodal_balance!(balance, c[:bus], nodal_balance(c))
nodal_balance(c::OnePort) = c[:p]


## Generator
function build(c::Generator)
    T = axis(c, :snapshots)
    G = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][g] <= p_nom[g=G] <= c[:p_nom_max][g]
    end

    @emvariable c c[:p_min_pu][g,t] * c[:p_nom][g] <= p[g=G,t=T] <= c[:p_max_pu][g,t] * c[:p_nom][g]
end

## StorageUnit
cost(c::StorageUnit) = sum(c[:marginal_cost] .* c[:p_dispatch]) + sum(c[:capital_cost] .* c[:p_nom])
nodal_balance(c::StorageUnit) = (p_dispatch = c[:p_dispatch]; AxisArray(p_dispatch .- c[:p_store], axes(p_dispatch)))
function build(c::StorageUnit)
    T = axis(c, :snapshots)
    S = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][s] <= p_nom[s=S] <= c[:p_nom_max][s]
    end

    @emvariable c 0 <= p_dispatch[s=S,t=T] <= c[:p_max_pu][s,t] * c[:p_nom][s]
    @emvariable c 0 <= p_store[s=S, t=T] <= - c[:p_min_pu][s,t] * c[:p_nom][s]
    @emvariable c 0 <= state_of_charge[s=S,t=T] <= c[:max_hours][s,t] * c[:p_nom][s]
    @emvariable c 0 <= spill[s=S,t=T] <= c[:inflow][s,t]

    soc_prev = circshift(c[:state_of_charge], :snapshots=>1)
    if !c[:cyclic_state_of_charge] soc_prev[:,T[1]] .= c[:state_of_charge_initial] end

    @emconstraint(c, soc_eq[s=S, t=T],
                  c[:state_of_charge][s,t] - soc_prev[s,t]
                  == c[:p_store][s,t] * c[:efficiency_store][s]
                  - c[:p_dispatch][s,t] / c[:efficiency_dispatch][s]
                  + c[:inflow][s,t] - c[:spill][s,t])
end

## Store
cost(c::Store) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:e_nom])
function build(c::Store)
    T = axis(c, :snapshots)
    S = axis(c)

    if isvar(c, :e_nom)
        @emvariable c c[:e_nom_min][s] <= e_nom[s=S] <= c[:e_nom_max][s]
    end

    @emvariable c c[:e_min_pu][s,t] * c[:e_nom][s] <= e[s=S,t=T] <= c[:e_max_pu][s,t] * c[:e_nom][s]
    @emvariable c p[s=S,t=T]

    e_prev = circshift(c[:e], :snapshots=>1)
    if !c[:e_cyclic] e_prev[:,T[1]] .= c[:e_initial] end

    @emconstraint(c, e_eq[s=S, t=T],
                  c[:e][s,t] - (1. - c[:standing_loss][s]) * e_prev[s,t] == c[:p])
end

## Load
cost(c::Load) = 0.
nodal_balance(c::Load) = (p = c[:p_set]; AxisArray(-p, axes(p)))
build(c::Load) = nothing


## Link
function add_nodal_balance!(balance, c::Link)
    p = c[:p]
    add_nodal_balance!(balance, c[:bus0], -p)
    add_nodal_balance!(balance, c[:bus1], AxisArray(c[:efficiency].*p, axes(p)))
end
cost(c::Link) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])
function build(c::Link)
    L = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][l] <= p_nom[l=L] <= c[:p_nom_max][l]
    end

    @emvariable c c[:p_min_pu][l,t] * c[:p_nom][l] <= p[l=L,t=:snapshots] <= c[:p_max_pu][l,t] * c[:p_nom][l]
end

## Line
cost(c::Line) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:s_nom])
function add_nodal_balance!(balance, c::Line)
    add_nodal_balance!(balance, c[:bus0], c[:p])
    add_nodal_balance!(balance, c[:bus1], -c[:p])
end

function build(c::PassiveBranch)
    L = axis(c)

    if isvar(c, :s_nom)
        @emvariable c c[:s_nom_min][l] <= s_nom[l=L] <= c[:s_nom_max][l]
    end

    @emvariable c -c[:s_max_pu][l,t] * c[:s_nom][l] <= p[l=L,t=:snapshots] <= c[:s_max_pu][l,t] * c[:s_nom][l]
end

function build(m::EnergyModel, c::Bus)
    T = axis(m, :snapshots)
    B = axis(c)
    bal = nodal_balance(m, B)
    @emconstraint(c, balance[B,T], bal .== 0)
end

function build(sn::SubNetwork)
    T = axis(sn.model, :snapshots)
    branches = sn[PassiveBranch]
    B = axis(branches)
    length(B) == 0 && return

    p = view(branches, :p, (B, T))
    effimp = mapcat(effectiveimpedance, branches)

    C = cycle_matrix(sn)
    Cl = rowvals(C)
    Cv = nonzeros(C)

    @emconstraint(sn, cycles[c=1:size(C,2),t=T], sum(Cv[j] * effimp[Cl[j]] * p[Cl[j],T] for j=nzrange(C,c)) == 0)
end

