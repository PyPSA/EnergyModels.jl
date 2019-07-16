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
    data::Dict{Symbol,AxisArray}
end
ComponentDesc(tup) = ComponentDesc(tup...)

struct Data{T} <: AbstractData
    components::Dict{Symbol,ComponentDesc}
    axes::Dict{Symbol,Axis}
    fallback::T
end

Data(; components=Dict{Symbol,ComponentDesc}(), axes=Dict{Symbol,Axis}(), fallback=nothing) = Data(components, axes, fallback)

AxisArrays.axes(cd::ComponentDesc) =
    Iterators.flatten(AxisArrays.axes(attr) for attr in values(cd.data))

function collectaxes(components; kwargs...)
    axdict = Dict{Symbol, Axis}()
    for cd in components
        for ax in AxisArrays.axes(cd; kwargs...)
            axname = axisname(ax)
            if haskey(axdict, axname)
                @assert axdict[axname] == ax
                # TODO the assert does not provide enough information to fix the problem!
            else
                axdict[axname] = ax
            end
        end
    end
    axdict
end

Data(components::Vararg{Tuple{Symbol,Any,Dict{Symbol,AxisArray}}}; kwargs...) = Data(ComponentDesc.(components); kwargs...)
function Data(components::Vector{ComponentDesc}; fallback=nothing)
    compdict = Dict(cd.name => cd for cd in components)
    axdict = collectaxes(components)

    Data(compdict, axdict, fallback)
end

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
