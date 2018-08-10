const ElementType = Type{<:ModelElement}
const elements = Dict{Symbol,ElementType}()

resolve(elemtype::Symbol) = elements[elemtype]
addelement(::Type{T}) where T<:ModelElement = (elements[naming(T)] = T)

function naming(T::ElementType)
    s = lowercase(string(T.name.name))
    Symbol(s, (s[end] == 's' ? "es" : "s"))
end
