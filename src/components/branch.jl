# Branch
"Connected to exactly two `Bus`es, determined by :bus0 and :bus1 attribute"
abstract type Branch <: Component end

# ActiveBranch
abstract type ActiveBranch <: Branch end

## Link
struct Link <: ActiveBranch
    model::EnergyModel
    class::Symbol
end

busattributes(c::Branch) = (:bus0, :bus1)

# Possible ways to have an emaggregator macro
# @emaggregator(nodalbalance, c::Link, l, c[:bus0] => c[:p][l], c[:bus1] => -c[:p][l])
# oder
# nodalbalance(c::Link) = @emaggregator(c, l, c[:bus0] => c[:p][l], c[:bus1] => -c[:p][l])

function nodalbalance(c::Link)
    p = c[:p]
    eff = c[:efficiency]
    (c[:bus0] => (l,t)->-p[l,t],
     c[:bus1] => (l,t)->eff[l]*p[l,t])
end

cost(c::Link) = sum(c[:marginal_cost] .* c[:p]) + sum(c[:capital_cost] .* c[:p_nom])
function build(c::Link)
    L = axis(c)

    if isvar(c, :p_nom)
        @emvariable c c[:p_nom_min][l] <= p_nom[l=L] <= c[:p_nom_max][l]
    end

    @emvariable c c[:p_min_pu][l,t] * c[:p_nom][l] <= p[l=L,t=:snapshots] <= c[:p_max_pu][l,t] * c[:p_nom][l]
end

addelement(Link)


# PassiveBranch
abstract type PassiveBranch<: Branch end
for component = (:Line, :Transformer)
    @eval begin
        struct $component <: PassiveBranch
            model::EnergyModel
            class::Symbol
        end
    end
end


## Lines and Transformers
effectiveimpedance(c::PassiveBranch) = impedance(c)[c[:carrier] == "DC" ? :r_pu_eff : :x_pu_eff]
phaseshift(c::PassiveBranch) = 0.

function impedance(c::Line)
    ax = axis(c)
    bus_v_nom = c.model[Bus][:v_nom]
    v_nom = bus_v_nom[indexin(c[:bus0], first(axisvalues(bus_v_nom)))]

    p = gettypeparams(c.model.data, c, c.class)
    if p !== nothing
        num_parallel = c[:num_parallel]
        length = c[:length]

        # line types
        x = AxisArray(p[:x_per_length] .* length ./ num_parallel, ax)
        r = AxisArray(p[:r_per_length] .* length ./ num_parallel, ax)
        # b = AxisArray(2pi*1e-9*p[:f_nom]*p[:c_per_length] .* length .* num_parallel, ax)
    else
      x = c[:x]
      r = c[:r]
    # b = c[:b]
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

phaseshift(c::Transformer) = gettypeparams(c.model.data, c, class)[:phase_shift]
function impedance(c::Transformer)
    p = gettypeparams(c.model.data, c, c.class)

    num_parallel = c[:num_parallel]

    r_pu = p[:vscr]/100 / p[:s_nom] ./ num_parallel
    x_pu = sqrt(p[:vsc]^2 - p[:vscr]^2)/100 / p[:s_nom] ./ num_parallel
    g_pu = p[:pfe]/1000 .* num_parallel
    b_pu = (-sqrt(clamp((p[:i0] * p[:s_nom])^2 - (p[:pfe]/10)^2, 0., Inf)) / 100
            .* num_parallel)

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

    c[:model] == "t" && apply_transformer_t_model!(r_pu, x_pu, g_pu, b_pu)

    tap_ratio = 1 .+ (c[:tap_position] .- p[:tap_neutral]) .* (p[:tap_step]/100.)
    Dict(:r_pu => r_pu, :r_pu_eff => r_pu .* tap_ratio,
         :x_pu => x_pu, :x_pu_eff => x_pu .* tap_ratio,
         :g_pu => g_pu, :b_pu => b_pu,
         :tap_ratio => tap_ratio,
         :phase_shift => p[:phase_shift],
         :s_nom => p[:s_nom])
end


cost(c::PassiveBranch) = sum(c[:capital_cost] .* c[:s_nom])

function nodalbalance(c::Branch)
    p = c[:p]
    (c[:bus0] => (b,t)->p[b,t], c[:bus1] => (b,t)->-p[b,t])
end

function build(c::PassiveBranch)
    L = axis(c)

    if isvar(c, :s_nom)
        @emvariable c c[:s_nom_min][l] <= s_nom[l=L] <= c[:s_nom_max][l]
    end

    @emvariable c -c[:s_max_pu][l,t] * c[:s_nom][l] <= p[l=L,t=:snapshots] <= c[:s_max_pu][l,t] * c[:s_nom][l]
end

addelement(Line)
addelement(Transformer)
