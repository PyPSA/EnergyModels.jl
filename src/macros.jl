macro consense(x, msgs...)
    msg = isempty(msgs) ? string(x, " has conflicting values") : :(string($(esc(msgs[1])), ": ", unique(v)))
    quote
        v = $(esc(x))
        @assert(all(broadcast(==, v, first(v))), $msg)
        first(v)
    end
end

macro adddevice(type, abstype, defaultformulation, name, axes, file)
    type = esc(type)
    abstype = esc(abstype)
    defaultformulation = esc(defaultformulation)
    quote
        struct $type{DF<:DeviceFormulation} <: $abstype{DF}
            model::EnergyModel
            class::Symbol
        end
        # Fall back to default formulation
        $type(model, class) = $type{$defaultformulation}(model, class)
        # Allow changing device formulation
        $type{DF}(d::$type) where DF = $type{DF}(d.model, d.class)
        $type{DF}(d::$type, ::Type{NewDF}) where {DF <: DeviceFormulation, NewDF <: DeviceFormulation} = $type{NewDF}(d)

        addcomponent($type, $(esc(name)), $(esc(axes)), $(esc(file)))
    end
end
