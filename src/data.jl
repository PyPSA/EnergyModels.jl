using NCDatasets

abstract type AbstractNcData <: Data end

include("data/pypsa.jl")

struct NcData <: Data
    dataset::Dataset
end

function NcData(filename::String)
    ds = Dataset(filename)
    (haskey(ds.attrib, "network_pypsa_version") ? PypsaNcData : NcData)(ds)
end

function load(filename::String)::Data
    if endswith(filename, ".nc")
        NcData(filename)
    else
        error("file ending of '$filename' not recognized")
    end
end

function Base.get(data::NcData, args...)
    da = data.dataset[naming(args...)]
    AxisArray(da[:], (axis(data, n) for n = dimnames(da))...)
end

axis(data::AbstractNcData, n::Symbol) = axis(data, String(n))
axis(data::AbstractNcData, n::String) = Axis{Symbol(n)}(disallowmissing(data.dataset[n][:]))

# Interface for the abstract type Data
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


struct DictData{T} <: Data
    axes::Dict{Symbol, Axis}
    data::Dict{Symbol, Dict{Symbol,AxisArray}}
    fallback::T
    variables::Dict{Symbol, Set}
    devices::Vector{Tuple{Symbol, Symbol}}
end

DictData(; variables=Dict{Symbol,Set}(), kwargs...) = DictData(Dict{Symbol,AxisArray}(kwargs), variables=variables)
function DictData(data::Dict{Symbol, AxisArray}; variables=Dict{Symbol,Set}())
    axs = Dict{Symbol, Axis}()
    for a = values(data),
        ax = axes(a)

        if haskey(axs, axisname(ax))
            @assert axs[axisname(ax)] == ax
        else
            axs[axisname(ax)] = ax
        end
    end

    devices = unique((Symbol.(split(string(k), "::")[[1,2]])...,) for k = keys(data))

    DictData(axs, data, variables, devices)
end

devices(data::DictData) = resolve.(unique(typ for (typ, class) = data.devices))
classes(data::DictData, T) = classes(data, naming(Symbol, T))
classes(data::DictData, ctype::Symbol) = (class for (typ, class) = data.devices if typ == ctype)
isvar(data::DictData, c::Device, class, quantity) = in(quantity, data.variables[naming(Symbol, c, class)])

function axis(data::DictData{<:Data}, n::Symbol)
    ret = get(data.axes, n, nothing)
    !isnothing(ret) ? ret : get(data.fallback, n)
end
axis(data::DictData{Nothing}, n::Symbol) = data.axes[n]
axis(data::DictData, c::Device) = axis(data, c.class)

Base.get(data::DictData{Nothing}, c::Component, quantity) = data.data[c.class][quantity]
function Base.get(data::DictData{<:Data}, c::Component, quantity)
    devicedict = get(data.data, c.class, nothing)
    if !isnothing(devicedict)
        ret = get(devicedict, quantity, nothing)
        !isnothing(ret) ? ret : get(data.fallback, c, quantity)
    else
        get(data.fallback, c, quantity)
    end
end
