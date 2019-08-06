module PowerSystemsData

import ..EnergyModels
const EM = EnergyModels

import PowerSystems
const PSY = PowerSystems

import TimeSeries
const TS = TimeSeries

using AxisArrays: AxisArray, Axis, axes

using ..EnergyModels:
    ComponentDesc, Data, resolve, Component, Device, DeviceFormulation,
    attributes, components, typenames, naming

function EM.naming(::Type{T}) where T <: PSY.Component
    s = lowercase(string(T.name.name))
    Symbol(s, in(s[end], ('s', 'h')) ? "es" : "s")
end

EM.load(sys::PSY.System) = read(sys)

function parselimits!(data, structs, fname, ax, pairs...)
    for (lim, attr) in pairs
        limit = [isnothing(f) ? nothing : f[lim]
                 for f in getfield.(structs, fname)]
        if !all(isnothing, limit)
            data[attr] = AxisArray(limit, ax)
        end
    end
end

function parse!(data, params::Vector{T}, ax) where T <: PSY.TechnicalParams
    for (fn, attr) in (:activepower => :p, :reactivepower => :q,
                       :capacity => :p_nom, :installedcapacity => :p_nom,
                       :curtailpenalty => :curtail_penalty,
                       :fixedcost => :fixed_cost, :variablecost => :marginal_cost,
                       :startupcost => :start_up_cost, :shutdncost => :shut_down_cost)
        if fn in fieldnames(T)
            d = AxisArray(getfield.(params, fn), ax)
            !all(isnothing, d) && (data[attr] = d)
        end
    end

    if :variablecost in fieldnames(T)
        d = getfield.(params, :variablecost)
        if !all(isnothing, d)
            # Linearizing cost!!!
            data[:marginal_cost] = AxisArray([i(1.0) for i in d], ax)
        end
    end

    for (fn, attrs) in (:activepowerlimits => (:min=>:p_min_pu, :max=>:p_max_pu),
                        :reactivepowerlimits => (:min=>:q_min_pu, :max=>:q_max_pu),
                        :ramplimits => (:up=>:ramp_limit_up, :down=>:ramp_limit_down),
                        :timelimits => (:up=>:min_up_time, :down=>:min_down_time))
        if fn in fieldnames(T)
            parselimits!(data, params, fn, ax, attrs...)
        end
    end
end

function rescale!(data, baseattr, attrs...)
    base = get(data, baseattr, nothing)
    isnothing(base) && return data
    for attr in attrs
        unscaled = get(data, attr, nothing)
        isnothing(unscaled) && continue
        data[attr] = AxisArray(unscaled ./ base, axes(unscaled)...)
    end
    data
end

function parse(sclfcts::Vector{T}, ax) where T <: TS.TimeArray
    timeseries = first((f for f in sclfcts if !isnothing(f)))
    snapshots = Axis{:snapshots}(TS.timestamp(timeseries))
    timeone = TS.TimeArray(snapshots.val, ones(length(snapshots)))
    AxisArray(hcat((values(something(f, timeone)) for f in sclfcts)...), snapshots, ax)
end

function parse(gens::Vector{T}) where T <: PSY.Generator
    name = naming(T)
    ax = Axis{name}(getfield.(gens, :name))
    data = Dict{Symbol,AxisArray}()
    data[:bus] = AxisArray([g.bus.name for g in gens], ax)
    :econ in fieldnames(T) && parse!(data, getfield.(gens, :econ), ax)
    :tech in fieldnames(T) && parse!(data, getfield.(gens, :tech), ax)
    rescale!(data, :p_nom, :p_max_pu, :p_min_pu)
    rescale!(data, :p_nom, :q_max_pu, :q_min_pu)

    if :scalingfactor in fieldnames(T)
        scalingfactor = parse(getfield.(gens, :scalingfactor), ax)
        p_max_pu = get(data, :p_max_pu, 1.0)
        data[:p_max_pu] = AxisArray(p_max_pu .* scalingfactor, axes(scalingfactor)...)
    end

    data
end

read(gens::Vector{T}) where T <: PSY.Generator =
    ComponentDesc(naming(T), EM.Generator{EM.LinearDispatchForm}, parse(gens))

read(gens::Vector{T}) where T <: PSY.ThermalGen =
    ComponentDesc(naming(T), EM.Generator{EM.UnitCommittmentForm}, parse(gens))

function read(gens::Vector{PSY.HydroStorage})
    data = parse(gens)
    data[:max_hours] = getfield.(gens, :storagecapacity) # TODO : Unit is m^3 ??!!
    ComponentDesc(:hydrostorageunits, EM.StorageUnit{EM.LinearDispatchForm}, data)
end

# Split inhomogeneous device types into separate devices
function read(devices::Vector{T}) where T <: PSY.Device
    isempty(devices) && return ComponentDesc[]
    if isconcretetype(T)
        error("Need to implement `read` for PowerSystems device type $T")
    end

    devicetypes = Dict{typeof(T),Vector}()
    for dev in devices
        typ = typeof(dev)
        push!(get!(devicetypes, typ, Vector{typ}()), dev)
    end
    map(read, values(devicetypes))
end

read(::Nothing) = ComponentDesc[]

function read(gens::PSY.GenClasses)
    [read(gens.thermal);
     read(gens.renewable);
     read(gens.hydro)]
end

function read(loads::Vector{PSY.PowerLoad})
    ax = Axis{:loads}(getfield.(loads, :name))
    scalingfactor = parse(getfield.(loads, :scalingfactor), ax)
    maxactivepower = reshape(getfield.(loads, :maxactivepower), 1, length(ax))
    data = Dict(
        :carrier => AxisArray(fill("AC", length(ax)), ax),
        :bus => AxisArray([l.bus.name for l in loads], ax),
        :p_set => AxisArray(maxactivepower .* scalingfactor,
                            axes(scalingfactor)...)
    )
    maxq = reshape(getfield.(loads, :maxreactivepower), 1, length(ax))
    if !all(isnothing, maxq)
        data[:q_set] = AxisArray(maxq .* scalingfactor, axes(scalingfactor))
    end

    ComponentDesc(:loads, EM.Load{EM.LinearDispatchForm}, data)
end

function read(buses::Vector{PSY.Bus})
    ax = Axis{:buses}(getfield.(buses, :name))
    data = Dict(
        :carrier => AxisArray(fill("AC", length(ax)), ax),
        :type => AxisArray(getfield.(buses, :bustype), ax),
        :v_nom => AxisArray(getfield.(buses, :basevoltage), ax),
        :v_ang => AxisArray(getfield.(buses, :angle), ax),
        :v_mag_pu_set => AxisArray(getfield.(buses, :voltage), ax),
        :v_mag_pu_min => AxisArray([b.voltagelimits.min for b in buses], ax),
        :v_mag_pu_max => AxisArray([b.voltagelimits.max for b in buses], ax)
    )
    ComponentDesc(:buses, EM.Bus, data)
end

function read(lines::Vector{PSY.Line})
    ax = Axis{:lines}(getfield.(lines, :name))

    data = Dict(
        :bus0 => AxisArray([l.connectionpoints.from.name for l in lines], ax),
        :bus1 => AxisArray([l.connectionpoints.to.name for l in lines], ax),
        :r => AxisArray(getfield.(lines, :r), ax),
        :x => AxisArray(getfield.(lines, :x), ax),
        :b => AxisArray([l.b.from + l.b.to for l in lines], ax)
    )

    parselimits!(data, lines, :anglelimits, ax, :min=>:v_ang_min, :max=>:v_ang_max)

    ComponentDesc(:lines, EM.Line{EM.LinearDispatchForm}, data)
end

function read(sys::PSY.System; kwargs...)
    components = vcat((read(getfield(sys, field))
                       for field in (:buses, :generators, :loads, :storage, :branches))...)::Vector{ComponentDesc}
    Data(components; kwargs...)
end

end

using .PowerSystemsData
