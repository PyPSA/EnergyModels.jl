# Distilled from JuMP's test/JuMPExtension.jl

"""
    ModelView{T,S}

A wrapper around any JuMP `model` to prefix variables and constraints with
`<component.class>::` and store them in an `component.objects`-Dictionary instead.
Every other operation is just handed through to the inner `model`.
"""
struct ModelView{T,S} <: JuMP.AbstractModel
    model::T
    component::S
end

prefix(view::ModelView, name::String) = string(view.component.class, "::", name)

Base.broadcastable(view::ModelView) = Ref(view)
JuMP.object_dictionary(view::ModelView) = view.component.objects

JuMP.variable_type(view::ModelView) = variable_type(view.model)
JuMP.add_variable(view::ModelView, v::JuMP.AbstractVariable, name::String="") = add_variable(view.model, v, prefix(view, name))
JuMP.delete(view::ModelView, variable_ref::VariableRef) = delete(view.model, variable_ref)
JuMP.is_valid(view::ModelView, variable_ref::VariableRef) = is_valid(view.model, variable_ref)
JuMP.num_variables(view::ModelView) = num_variables(view.model)

JuMP.constraint_type(view::ModelView) = constraint_type(view.model)
JuMP.add_constraint(view::ModelView, c::JuMP.AbstractConstraint, name::String="") = add_constraint(view.model, c, prefix(view, name))
JuMP.delete(view::ModelView, constraint_ref::ConstraintRef) = delete(view.model, constraint_ref)
JuMP.is_valid(view::ModelView, constraint_ref::ConstraintRef) = is_valid(view.model, constraint_ref)
JuMP.num_constraints(view::ModelView) = num_constraints(view.model)

# Objective
JuMP.set_objective(view::ModelView, sense::MOI.OptimizationSense, f::JuMP.AbstractJuMPScalar) = set_objective(view.model, sense, f)
JuMP.set_objective_sense(view::ModelView, sense) = set_objective_sense(view.model, sense)
JuMP.objective_sense(view::ModelView) = objective_sense(view.model)

JuMP.objective_function_type(view::ModelView) = objective_function_type(view.model)
JuMP.objective_function(view::ModelView) = objective_function(view.model)

# Names
JuMP.variable_by_name(view::ModelView, name::String) = variable_by_name(view.model, prefix(view, name))
JuMP.constraint_by_name(view::ModelView, name::String) = constraint_by_name(view.model, prefix(view, name))

# Show
JuMP.show_backend_summary(io::IO, view::ModelView) = show_backend_summary(io, view.model)
JuMP.show_objective_function_summary(io::IO, view::ModelView) = show_objective_function_sumary(io, view.model)
JuMP.objective_function_string(print_mode, view::ModelView) = objective_function_string(print_mode, view.model)
JuMP.show_constraints_summary(io::IO, view::ModelView) = show_constraints_summary(io, view.model)
JuMP.constraints_string(print_mode, view::ModelView) = constraints_string(print_mode, view.model)
