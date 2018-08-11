const ElementType = Type{<:ModelElement}
struct ElementAttributes
    elemtype::ElementType
    attributes::DataFrame
end

const elemtypenames = Dict{ElementType,Symbol}()
const elements = Dict{Symbol,ElementAttributes}()

resolve(elemtypename::Symbol) = elements[elemtypename].elemtype
attributes(elemtypename::Symbol) = elements[elemtypename].attributes

addelement(::Type{T}) where T<:ModelElement = addelement(T, naming(T))
addelement(::Type{T}, name::Symbol) where T<:ModelElement = addelement(T, name, ElementAttributes(T, DataFrame()))
addelement(::Type{T}, name::Symbol, eq::ElementAttributes) where T<:ModelElement =
    (elements[name] = eq; elemtypenames[T] = name)

function addelement(::Type{T}, name::Symbol, axes, filename) where T<:ModelElement
    axes = (first(axes)=>name, Base.tail(axes)...)
    df =  CSV.read(filename, truestring="t", falsestring="f")
    df[:attribute] = Symbol.(df[:attribute])
    df[:default] = map(r->typeparsers[r[:dtype]](r[:default]), eachrow(df))
    rename_dimensions(x) = tuple(recode(Symbol.(split(x, ',')), axes...)...)
    df[:dimensions] = map(x->ismissing(x) ? () : rename_dimensions(x), df[:dimensions])

    addelement(T, name, ElementAttributes(T, df))
end

function naming(T::ElementType)
    haskey(elemtypenames, T) && return elemtypenames[T]
    s = lowercase(string(T.name.name))
    Symbol(s, (s[end] == 's' ? "es" : "s"))
end
