# PassiveBranch
abstract type PassiveBranch{DF<:DeviceFormulation} <: Branch{DF} end

cost(d::PassiveBranch) = sum(d[:capital_cost] .* (d[:s_nom] .- getparam(d, :s_nom)))

# Split lines into AC and DC?
@adddevice(Line, PassiveBranch, LinearExpansionForm{LinearDispatchForm},
           :lines, (:L, :T=>:snapshots), joinpath(@__DIR__, "attrs", "lines.csv"))

@adddevice(Transformer, PassiveBranch, LinearExpansionForm{LinearDispatchForm},
           :transformers, (:L, :T=>:snapshots), joinpath(@__DIR__, "attrs", "transformers.csv"))

function addto!(jm::ModelView, m::EnergyModel, d::PassiveBranch{DF}) where
      {DDF, DF <: LinearExpansionForm{DDF}}

    addto!(jm, m, with_formulation(d, LinearExpansionInvestmentForm))
    addto!(jm, m, with_formulation(d, LinearExpansionDispatchForm{DDF}))
end


function addto!(jm::ModelView, m::EnergyModel, d::PassiveBranch{DF}) where
      {DF <: LinearExpansionInvestmentForm}

    L = axis(m, d)

    s_nom_min = get(d, :s_nom_min, L)
    s_nom_max = get(d, :s_nom_max, L)

    @variable(jm, s_nom_min[l] <= s_nom[l=L] <= s_nom_max[l])
end

function addto!(jm::ModelView, m::EnergyModel, d::PassiveBranch{DF}) where
      {DDF, DF <: LinearExpansionDispatchForm{DDF}}

    L = axis(m, d)
    T = axis(m, :snapshots)

    s_nom = get(d, :s_nom, L)
    s_max_pu = get(d, :s_max_pu, L, T)

    @variable(jm, p[L,T])

    @constraints jm begin
        p_upper[l=L,t=T], p[l,t] <= s_max_pu[l,t] * s_nom[l]
        p_lower[l=L,t=T], p[l,t] >= - s_max_pu[l,t] * s_nom[l]
    end
end


function addto!(jm::ModelView, m::EnergyModel, d::PassiveBranch{DF}) where {DF <: DispatchForm}

    L = axis(m, d)
    T = axis(m, :snapshots)

    s_nom = get(d, :s_nom, L)
    s_max_pu = get(d, :s_max_pu, L, T)

    @variable(jm, - s_max_pu[l,t] * s_nom[l] <= p[l=L,t=T] <= s_max_pu[l,t] * s_nom[l])
end

## Lines and Transformers
effectiveimpedance(d::PassiveBranch) = impedance(d)[d[:carrier] == :DC ? :r_pu_eff : :x_pu_eff]

phaseshift(d::PassiveBranch) = 0.
phaseshift(d::Transformer) = gettypeparams(d.model.data, d)[:phase_shift]

function impedance(d::Line)
    L = axis(d)
    bus_v_nom = d.model[Bus][:v_nom]
    v_nom = bus_v_nom[convert(Array{Int64}, indexin(get(d, :bus0, L), first(axisvalues(bus_v_nom))))]

    p = gettypeparams(d.model.data, d)
    if p !== nothing
        num_parallel = get(d, :num_parallel, L)
        length = get(d, :length, L)

        # line types
        x = AxisArray(p[:x_per_length] .* length ./ num_parallel, L)
        r = AxisArray(p[:r_per_length] .* length ./ num_parallel, L)
        # b = AxisArray(2pi*1e-9*p[:f_nom]*p[:d_per_length] .* length .* num_parallel, ax)
    else
        x = get(d, :x, L)
        r = get(d, :r, L)
        # b = d[:b]
    end

    x_pu = AxisArray(x ./ v_nom.^2, L)
    r_pu = AxisArray(r ./ v_nom.^2, L)
    # b_pu = AxisArray(r .* v_nom.^2, ax)

    Dict(:x => x, :r => r,
         # :b => b, :g => g,
         :x_pu => x_pu, :x_pu_eff => x_pu,
         :r_pu => r_pu, :r_pu_eff => r_pu,
         # :b_pu => b_pu, :g_pu => 0
         )
end

function impedance(d::Transformer)
    ax = axis(d)
    p = gettypeparams(d.model.data, d)

    if p !== nothing
        num_parallel = d[:num_parallel]
        s_nom = p[:s_nom]

        r = p[:vscr]/100 ./ num_parallel
        x = sqrt(p[:vsc]^2 - p[:vscr]^2)/100 ./ num_parallel
        g = p[:pfe]/(1000*p[:s_nom]) .* num_parallel
        b = (-sqrt(clamp((p[:i0])^2 - (p[:pfe]/(10*p[:s_nom]))^2, 0., Inf)) / 100
             .* num_parallel)
    else
        s_nom = d[:s_nom]

        x = d[:x]
        r = d[:r]
        g = d[:g]
        b = d[:b]
    end

    r_pu = AxisArray(r ./ s_nom, ax)
    x_pu = AxisArray(x ./ s_nom, ax)
    g_pu = AxisArray(g .* s_nom, ax)
    b_pu = AxisArray(b .* s_nom, ax)

    """Follows http://home.earthlink.net/~w6rmk/math/wyedelta.htm"""
    function wye_to_delta(z1,z2,z3)
        summand = z1*z2 + z2*z3 + z3*z1
        (summand/z2,summand/z1,summand/z3)
    end

    """Convert given T-model parameters to PI-model parameters using wye-delta transformation"""
    function apply_transformer_t_model!(r_pu, x_pu, g_pu, b_pu)
        for i in eachindex(r_pu, x_pu, g_pu, b_pu)
            z_series = r_pu[i] + im*x_pu[i]
            y_shunt  = g_pu[i] + im*b_pu[i]

            if y_shunt == 0 continue end

            za, zb, zc = wye_to_delta(z_series/2, z_series/2, 1/y_shunt)

            r_pu[i] = real(zc)
            x_pu[i] = imag(zc)
            g_pu[i] = real(2/za)
            b_pu[i] = imag(2/za)
        end
    end

    if d[:model] == "t"
        apply_transformer_t_model!(r_pu, x_pu, g_pu, b_pu)
    end

    tap_ratio = AxisArray(1 .+ (d[:tap_position] .- p[:tap_neutral]) .* (p[:tap_step]/100.), ax)
    Dict(:r_pu => r_pu, :r_pu_eff => AxisArray(r_pu .* tap_ratio, ax),
         :x_pu => x_pu, :x_pu_eff => AxisArray(x_pu .* tap_ratio, ax),
         :g_pu => g_pu, :b_pu => b_pu,
         :tap_ratio => tap_ratio,
         :phase_shift => p[:phase_shift],
         :s_nom => p[:s_nom])
end
