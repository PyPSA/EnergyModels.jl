EnergyModel(filename::String; kwargs...) = EnergyModel(load(filename); kwargs...)
EnergyModel(data::Data; solver=GurobiSolver()) = load(EnergyModel(Dict{Symbol,Component}(), data, Model(solver=solver)))

function load(m::EnergyModel)
    for T = components(m.data), clas = classes(m.data, T)
        m.components[Symbol(string(naming(T), "::", clas))] = T(m, clas, Dict{Symbol}{Any}(), Dict{Symbol}{Any}())
    end
    m
end

Base.show(io::IO, c::Component) = print(io, typeof(c), " for class ", c.class)
function Base.show(io::IO, ::MIME"text/plain", c::Component)
    println(io, c, " with ")
    println(io, "* ", length(c.vars), " variables")
    print(io, "* ", length(c.constrs), " constraints")
end


build(::Model, ::Component) = error("Not implemented")
build(::Component) = error("Not implemented")

jumpmodel(c::Component) = c.model.jump
expression(c::Component, ::ExpressionType) = error("Not implemented")
expression(c::Component, ::Cost) = cost(c)
cost(::Component) = error("Not implemented")

Base.getindex(c::Component, attr::Symbol) = haskey(c.vars, attr) ? c.vars[attr] : get(c.model.data, c, c.class, attr)

# TODO Use the indexsets information to make sure, we're getting the correct bit
view(c::Component, attr::Symbol, axes::Vector{Axis}) = WrappedArray(c[attr], axes...)

axis(c::Component) = axis(c.model.data, c, c.class)
axis(c::Component, attr) = axis(c.model.data, attr)

cost(m::Model, c::Generator) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])

function build(c::Generator)
  G = axis(c, :generators)
  T = axis(c, :snapshots)

  if c[:p_nom_extendable]
      @emvariable c c[:p_nom_min][g] <= p_nom[g=G] <= c[:p_nom_max][g]
      @emvariable c p[g=G,t=T]
      @emconstraint c balancing_lb[g=G,t=T] p[g,t] >= c[:p_min_pu][g,t] * c[:p_nom][g]
      @emconstraint c balancing_ub[g=G,t=T] p[g,t] <= c[:p_max_pu][g,t] * c[:p_nom][g]
  else
      @emvariable c c[:p_min_pu][g,t] * c[:p_nom][g] <= p[g=G,t=T] <= c[:p_min_pu][g,t] * c[:p_nom][g]
  end
end
