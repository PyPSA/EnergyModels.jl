const ElementType = Type{<:ModelElement}
struct ElementQuantities
    elemtype::ElementType
    quantities::DataFrame
end

const elemtypenames = Dict{ElementType,Symbol}()
const elements = Dict{Symbol,ElementQuantities}()

resolve(elemtypename::Symbol) = elements[elemtypename].elemtype
quantities(elemtypename::Symbol) = elements[elemtypename].quantities

addelement(::Type{T}) where T<:ModelElement = addelement(T, naming(T))
addelement(::Type{T}, name::Symbol) where T<:ModelElement = addelement(T, name, ElementQuantities(T, DataFrame()))
addelement(::Type{T}, name::Symbol, eq::ElementQuantities) where T<:ModelElement =
    (elements[name] = eq; elemtypenames[T] = name)

function addelement(::Type{T}, name::Symbol, axes, filename) where T<:ModelElement
    axes = (first(axes)=>name, Base.tail(axes)...)
    df =  CSV.read(filename, truestring="t", falsestring="f")
    df[:default] = map(r->typeparsers[r[:dtype]](r[:default]), eachrow(df))

    rename_dimensions(x) = tuple(recode(Symbol.(split(x, ',')), axes...)...)
    df[:dimensions] = categorical(map(x->ismissing(x) ? () : rename_dimensions(x), df[:dimensions]))

    addelement(T, name, ElementQuantities(T, df))
end

function naming(T::ElementType)
    haskey(elemtypenames, T) && return elemtypenames[T]
    s = lowercase(string(T.name.name))
    Symbol(s, (s[end] == 's' ? "es" : "s"))
end
