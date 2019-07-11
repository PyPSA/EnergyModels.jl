# Distilled from JuMP's test/JuMPExtension.jl

"""
    ModelView{T,S}

A wrapper around any JuMP `model` to prefix variables and constraints with
`<component.class>::` and store them in an `component.objects`-Dictionary instead.
Every other operation is just handed through to the inner `model`.
"""
struct ModelView{T,S} <: JuMP.AbstractModel
    jumpmodel::T
    model::S
    class::Symbol
end

ModelView(jumpmodel::JuMP.AbstractModel, c::Component) = ModelView(jumpmodel, model(c), c.class)

prefix(view::ModelView, name::String) = string(view.class, "::", name)

Base.broadcastable(view::ModelView) = Ref(view)
JuMP.object_dictionary(view::ModelView) = get!(view.model.jumpobjects, view.class, Dict{Symbol}{Any}())

JuMP.variable_type(view::ModelView) = variable_type(view.jumpmodel)
JuMP.add_variable(view::ModelView, v::JuMP.AbstractVariable, name::String="") =
    view.model.jumpnames ? add_variable(view.jumpmodel, v, prefix(view, name)) : add_variable(view.jumpmodel, v)
JuMP.delete(view::ModelView, variable_ref::VariableRef) = delete(view.jumpmodel, variable_ref)
JuMP.is_valid(view::ModelView, variable_ref::VariableRef) = is_valid(view.jumpmodel, variable_ref)
JuMP.num_variables(view::ModelView) = num_variables(view.jumpmodel)

JuMP.constraint_type(view::ModelView) = constraint_type(view.jumpmodel)
JuMP.add_constraint(view::ModelView, c::JuMP.AbstractConstraint, name::String="") =
    view.model.jumpnames ? add_constraint(view.jumpmodel, c, prefix(view, name)) : add_constraint(view.jumpmodel, c)
JuMP.delete(view::ModelView, constraint_ref::ConstraintRef) = delete(view.jumpmodel, constraint_ref)
JuMP.is_valid(view::ModelView, constraint_ref::ConstraintRef) = is_valid(view.jumpmodel, constraint_ref)
JuMP.num_constraints(view::ModelView) = num_constraints(view.jumpmodel)

# Objective
JuMP.set_objective(view::ModelView, sense::MOI.OptimizationSense, f::JuMP.AbstractJuMPScalar) = set_objective(view.jumpmodel, sense, f)
JuMP.set_objective_sense(view::ModelView, sense) = set_objective_sense(view.jumpmodel, sense)
JuMP.objective_sense(view::ModelView) = objective_sense(view.jumpmodel)

JuMP.objective_function_type(view::ModelView) = objective_function_type(view.jumpmodel)
JuMP.objective_function(view::ModelView) = objective_function(view.jumpmodel)

# Names
JuMP.variable_by_name(view::ModelView, name::String) =
    view.model.jumpnames ? variable_by_name(view.jumpmodel, prefix(view, name)) : nothing
JuMP.constraint_by_name(view::ModelView, name::String) =
    view.model.jumpnames ? constraint_by_name(view.jumpmodel, prefix(view, name)) : nothing

# Show
JuMP.show_backend_summary(io::IO, view::ModelView) = show_backend_summary(io, view.jumpmodel)
JuMP.show_objective_function_summary(io::IO, view::ModelView) = show_objective_function_sumary(io, view.jumpmodel)
JuMP.objective_function_string(print_mode, view::ModelView) = objective_function_string(print_mode, view.jumpmodel)
JuMP.show_constraints_summary(io::IO, view::ModelView) = show_constraints_summary(io, view.jumpmodel)
JuMP.constraints_string(print_mode, view::ModelView) = constraints_string(print_mode, view.jumpmodel)
