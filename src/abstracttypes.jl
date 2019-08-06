abstract type AbstractEnergyModel end

abstract type AbstractData end

abstract type Expression end


"Can be `add`ed to a JuMP model, must have an `objects` dictionary and a `class`"
abstract type Component end

abstract type DeviceFormulation end

"Connected to at least one `Bus`. Additionally to `addto!` provides `p`, `cost` and `busattributes`"
abstract type Device{DF<:DeviceFormulation} <: Component end

abstract type ModelType end

# Expressions

abstract type Emission <: Expression end
struct Cost <: Expression end
struct CO2 <: Emission end
struct NodalActivePower <: Expression end
struct NodalReactivePower <: Expression end

