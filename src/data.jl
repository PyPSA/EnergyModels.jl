using NCDatasets

abstract type AbstractNcData <: AbstractData end

gettypeparams(data::AbstractData, c::Component) = nothing
axis(data::AbstractData, c::Component) = axis(data, naming(c))

function components end

struct NcData <: AbstractNcData
    dataset::Dataset
end

function NcData(filename::String)
    ds = Dataset(filename)
    (haskey(ds.attrib, "network_pypsa_version") ? PypsaNcData : NcData)(ds)
end

function load(filename::String)::AbstractData
    if endswith(filename, ".nc")
        Data(fallback=NcData(filename))
    else
        error("file ending of '$filename' not recognized")
    end
end

function Base.get(data::NcData, args...)
    da = data.dataset[String(naming(args...))]
    AxisArray(da[:], (axis(data, n) for n = dimnames(da))...)
end

axis(data::AbstractNcData, n::Symbol) = Axis{n}(nomissing(data.dataset[String(n)][:]))

# Interface for the abstract type AbstractData
#
# * Required methods
#
# Base.get(data, class, param)
# returns an AxisArray of `param` from `class`
#
# devices(data)
# returns Array{DataType} with the described devices
#
# classes(data, ::Type{T}) where T<:Device
# returns Array{Symbol} of the described classes

struct ComponentDesc
    name::Symbol
    componenttype
    data::Dict{Symbol,Union{Scalar,AxisArray}}
end
ComponentDesc(tup) = ComponentDesc(tup...)

struct Data{T} <: AbstractData
    components::Dict{Symbol,ComponentDesc}
    axes::Dict{Symbol,Axis}
    fallback::T

    function Data(components::Dict{Symbol,ComponentDesc}, fallback::Union{Nothing,AbstractData})
        data = new{typeof(fallback)}(components, Dict{Symbol,Axis}(), fallback)
        for cd in values(data.components)
            pushaxes!(data, cd)
        end
        data
    end
end
Data(; components=Dict{Symbol,ComponentDesc}(), fallback=nothing) = Data(components, fallback)
Data(components::Vector{ComponentDesc}; fallback=nothing) = Data(Dict(cd.name => cd for cd in components), fallback)
Data(components::Vararg{Tuple{Symbol,Any,Dict{Symbol}}}; kwargs...) = Data(ComponentDesc.(components); kwargs...)

function pushaxes!(data::AbstractData, ax::Axis; component="unknown", attribute="unknown")
    ## Duck-typing on that data has an `axes` dictionary since we want to use this function for PypsaNcData as well
    axname = axisname(ax)
    if haskey(data.axes, axname)
        if data.axes[axname] != ax
            error("Incompatible axes: The $attribute attribute of component $component has the axis $axname, which we previously encountered with different values:\n\t$(data.axes[axname].val) != $(ax.val) (previous != new)")
        end
    else
        data.axes[axname] = ax
    end
end

function pushaxes!(data::Data, cd::ComponentDesc)
    for (name, attr) in cd.data
        if isa(attr, AxisArray)
            for ax in AxisArrays.axes(attr)
                pushaxes!(data, ax, component=cd.name, attribute=name)
            end
        else
            pushaxes!(data, Axis{name}([name]), component=cd.name, attribute=name)
        end
    end
end

Base.push!(data::Data, cd, ax::Axis{name}) where name = push!(data, cd, Axis{name}(Symbol.(ax.val)))
function Base.push!(data::Data, cd::ComponentDesc, ax::Axis{name,Array{Symbol,1}}) where name
    if name != cd.name
        @warn "Renaming principal axis $(AxisArrays.axisname(ax)) to $(cd.name)"
        ax = Axis{cd.name}(ax.val)
    end
    data.components[cd.name] = cd
    data.axes[cd.name] = ax # We overwrite, since we overwrite the component as well
    pushaxes!(data, cd)
end

Base.push!(data::Data, ::Type{T}, ax::Axis{name}; parameters...) where {name,T <: Component} =
    push!(data, ComponentDesc(name, T, Dict(parameters)), ax)


_components(data) = map(cd->(cd.name => cd.componenttype), values(data.components))
components(data::Data{Nothing}) = _components(data)
components(data::Data{<:AbstractData}) =
    unique(p->p.first, vcat(components(data.fallback), _components(data)))

function axis(data::Data{<:AbstractData}, n::Symbol)
    ret = get(data.axes, n, nothing)
    !isnothing(ret) ? ret : axis(data.fallback, n)
end
axis(data::Data{Nothing}, n::Symbol) = data.axes[n]

function Base.get(data::Data{Nothing}, c::Component, quantity)
    ret = get(data.components[naming(c)].data, quantity, nothing)
    !isnothing(ret) ? ret : getdefault(c, quantity)
end
function Base.get(data::Data{<:AbstractData}, c::Component, quantity)
    componentdesc = get(data.components, naming(c), nothing)
    if !isnothing(componentdesc)
        ret = get(componentdesc.data, quantity, nothing)
        if isnothing(ret)
            ret = get(data.fallback, c, quantity)
        end
    else
        ret = get(data.fallback, c, quantity)
    end

    !isnothing(ret) ? ret : getdefault(c, quantity)
end

gettypeparams(data::Data{Nothing}, device::Device) = nothing
gettypeparams(data::Data{<:AbstractData}, device::Device) = gettypeparams(data.fallback, device)

include("data/pypsa.jl")
