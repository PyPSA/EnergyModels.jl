## Formulations for Devices

# Dispatch formulations
abstract type DispatchForm <: DeviceFormulation end

struct LinearDispatchForm <: DispatchForm end
struct UnitCommittmentForm <: DispatchForm end

# Expansion Formulations
abstract type ExpansionForm{T<:DispatchForm} <: DeviceFormulation end
struct LinearExpansionForm{T<:DispatchForm} <: ExpansionForm{T} end
struct LinearExpansionDispatchForm{T<:DispatchForm} <: ExpansionForm{T} end
struct LinearExpansionInvestmentForm{T<:DispatchForm} <: ExpansionForm{T} end

struct BlockExpansionForm{T<:DispatchForm} <: ExpansionForm{T} end
struct BlockExpansionDispatchForm{T<:DispatchForm} <: ExpansionForm{T} end
struct BlockExpansionInvestmentForm{T<:DispatchForm} <: ExpansionForm{T} end

struct BinaryExpansionForm{T<:DispatchForm} <: ExpansionForm{T} end
struct BinaryExpansionDispatchForm{T<:DispatchForm} <: ExpansionForm{T} end
struct BinaryExpansionInvestmentForm{T<:DispatchForm} <: ExpansionForm{T} end

struct FormulationDescription
    name::Symbol
    form::DataType
end

FormulationDescription(tup) = FormulationDescription(tup...)

const formulations = FormulationDescription.([
    (:lindisp, LinearDispatchForm),
    (:ucdisp, UnitCommittmentForm),
    (:linexp_lindisp, LinearExpansionForm{LinearDispatchForm}),
    (:linexp_ucdisp, LinearExpansionForm{UnitCommittmentForm})
])

function resolve(::Type{DeviceFormulation}, name::Symbol)
    j = findfirst(fd -> fd.name == name, formulations)
    !isnothing(j) ? formulations[j].form : error("Formulation $name has not been registered in `formulations`")
end

function naming(::Type{T}) where T <: DeviceFormulation
    j = findfirst(fd -> fd.form == T, formulations)
    !isnothing(j) ? formulations[j].name : error("Formulation $T has not been registered in `formulations`")
end

formulation(::Device{DF}) where DF <: DeviceFormulation = DF



# TODO multi-year formulations

## ModelTypes

abstract type DispatchModel <: ModelType end
struct LinearDispatchModel <: DispatchModel end
struct UnitCommittmentModel <: DispatchModel end

abstract type ExpansionModel <: ModelType end

# If nothing else is said, let's say it's fine
demote_formulation(::Type{ExpansionModel}, ::Type{DF}) where DF <: ExpansionForm = DF
demote_formulation(::Type{ExpansionModel}, ::Type{DF}) where DF <: DispatchForm = DF
demote_formulation(::Type{DispatchModel},  ::Type{DF}) where DF <: DispatchForm = DF

# An expansion form should demote to its respective DispatchForm
demote_formulation(::Type{MT}, ::Type{DF}) where {MT <: DispatchModel, DDF, DF <: ExpansionForm{DDF}} = demote_formulation(MT, DDF)

# We demote UnitCommittment to LinearDispatch in a LinearDispatchModel
demote_formulation(::Type{LinearDispatchModel}, ::Type{UnitCommittmentForm}) = LinearDispatchForm

function demote_formulation(m::EnergyModel{MT}, d::Device{DF}) where {MT <: ModelType, DF <: DeviceFormulation}
    # TODO test whether conditionally changing the type would be faster!
    with_formulation(d, demote_formulation(MT, DF))
end

with_formulation(d::T, ::Type{DF}) where {T <: Device, DF <: DeviceFormulation} = T(d, DF)
