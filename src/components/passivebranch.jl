# PassiveBranch
abstract type PassiveBranch<: Branch end

cost(d::PassiveBranch) = sum(d[:capital_cost] .* (AxisArray(d[:s_nom]) .- getparam(d, :s_nom)))

## Lines and Transformers
effectiveimpedance(d::PassiveBranch) = impedance(d)[d[:carrier] == "DC" ? :r_pu_eff : :x_pu_eff]
phaseshift(d::PassiveBranch) = 0.

@adddevice(Line, PassiveBranch, :lines, (:L, :T=>:snapshots), joinpath(@__DIR__, "attrs", "lines.csv"))

@adddevice(Transformer, PassiveBranch, :transformers, (:L, :T=>:snapshots), joinpath(@__DIR__, "attrs", "transformers.csv"))

function impedance(d::Line)
    ax = axis(d)
    bus_v_nom = d.model[Bus][:v_nom]
    v_nom = bus_v_nom[convert(Array{Int64}, indexin(d[:bus0], first(axisvalues(bus_v_nom))))]

    p = gettypeparams(d.model.data, d, d.class)
    if p !== nothing
        num_parallel = d[:num_parallel]
        length = d[:length]

        # line types
        x = AxisArray(p[:x_per_length] .* length ./ num_parallel, ax)
        r = AxisArray(p[:r_per_length] .* length ./ num_parallel, ax)
        # b = AxisArray(2pi*1e-9*p[:f_nom]*p[:d_per_length] .* length .* num_parallel, ax)
    else
        x = d[:x]
        r = d[:r]
        # b = d[:b]
    end

    x_pu = AxisArray(x ./ v_nom.^2, ax)
    r_pu = AxisArray(r ./ v_nom.^2, ax)
    # b_pu = AxisArray(r .* v_nom.^2, ax)

    Dict(:x => x, :r => r,
         # :b => b, :g => g,
         :x_pu => x_pu, :x_pu_eff => x_pu,
         :r_pu => r_pu, :r_pu_eff => r_pu,
         # :b_pu => b_pu, :g_pu => 0
         )
end

function addto!(jm::ModelView, m::EnergyModel, d::PassiveBranch)
    T = axis(m, :snapshots)
    L = axis(d)

    s_max_pu = get(d, :s_max_pu, L, T)

    if isvar(d, :s_nom)
        s_nom_min = get(d, :s_nom_min, L)
        s_nom_max = get(d, :s_nom_max, L)

        @variables jm begin
            s_nom_min[l] <= s_nom[l=L] <= s_nom_max[l]
            p[l=L,t=T]
        end

        @constraints jm begin
            p_upper[l=L,t=T], p[l,t] <= s_max_pu[l,t] * s_nom[l]
            p_lower[l=L,t=T], p[l,t] >= - s_max_pu[l,t] * s_nom[l]
        end
    else
        s_nom = get(d, :s_nom, L)
        @variable(jm, - s_max_pu[l,t] * s_nom[l] <= p[l=L,t=T] <= s_max_pu[l,t] * s_nom[l])
    end
end


phaseshift(d::Transformer) = gettypeparams(d.model.data, d, class)[:phase_shift]
function impedance(d::Transformer)
    ax = axis(d)
    p = gettypeparams(d.model.data, d, d.class)

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
