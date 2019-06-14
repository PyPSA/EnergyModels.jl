const ComponentType = Type{<:Component}
struct ComponentAttributes
    comptype::ComponentType
    attributes::DataFrame
end

const comptypenames = Dict{ComponentType,Symbol}()
const components = Dict{Symbol,ComponentAttributes}()

resolve(comptypename::Symbol) = components[comptypename].comptype
attributes(comptypename::Symbol) = components[comptypename].attributes

addcomponent(::Type{T}) where T<:Component = addcomponent(T, naming(T))
addcomponent(::Type{T}, name::Symbol) where T<:Component = addcomponent(T, name, ComponentAttributes(T, DataFrame()))
addcomponent(::Type{T}, name::Symbol, eq::ComponentAttributes) where T<:Component =
    (components[name] = eq; comptypenames[T] = name)

function addcomponent(::Type{T}, name::Symbol, axes, filename) where T<:Component
    axes = (first(axes)=>name, Base.tail(axes)...)
    df = CSV.read(filename, truestrings=["t"], falsestrings=["f"])
    df[:attribute] = Symbol.(df[:attribute])
    df[:default] = map(r->astype(r[:dtype], r[:default]), eachrow(df))
    rename_dimensions(x) = tuple(recode(Symbol.(split(x, ',')), axes...)...)
    df[:dimensions] = map(x->ismissing(x) ? () : rename_dimensions(x), df[:dimensions])

    addcomponent(T, name, ComponentAttributes(T, df))
end

function naming(T::ComponentType)
    haskey(comptypenames, T) && return comptypenames[T]
    s = lowercase(string(T.name.name))
    Symbol(s, in(s[end], ('s', 'h')) ? "es" : "s")
end
