macro consense(x, msgs...)
    msg = isempty(msgs) ? string(x, " has conflicting values") : :(string($(esc(msgs[1])), ": ", unique(v)))
    quote
        v = $(esc(x))
        @assert(all(broadcast(==, v, first(v))), $msg)
        first(v)
    end
end

macro adddevice(type, abstype, name, axes, file)
    type = esc(type)
    quote
        struct $type{DF<:DeviceFormulation} <: $(esc(abstype))
            model::EnergyModel
            class::Symbol
            objects::Dict{Symbol,Any}
        end
        $type{DF}(d::$type) where DF = $type{DF}(d.model, d.class, d.objects)
        $type{DF}(d::$type, ::Type{NewDF}) where {DF, NewDF <: DeviceFormulation} = $type{NewDF}(d)

        addcomponent($type, $(esc(name)), $(esc(axes)), $(esc(file)))
    end
end
