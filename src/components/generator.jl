## Generator
@adddevice(Generator, OnePort, :generators, (:G, :T=>:snapshots), joinpath(@__DIR__, "attrs", "generators.csv"))

function addto!(jm::ModelView, m::EnergyModel, d::Generator)
    T = axis(d, :snapshots)
    G = axis(d)

    p_min_pu = get(d, :p_min_pu, G, T)
    p_max_pu = get(d, :p_max_pu, G, T)

    if isvar(d, :p_nom)
        p_nom_min = get(d, :p_nom_min, G)
        p_nom_max = get(d, :p_nom_max, G)
        @variables jm begin
            p_nom_min[g] <= p_nom[g=G] <= p_nom_max[g]
            p[g=G,t=T]
        end

        @constraints jm begin
            p_lower[g=G,t=T], p[g,t] >= p_min_pu[g,t] * p_nom[g]
            p_upper[g=G,t=T], p[g,t] <= p_max_pu[g,t] * p_nom[g]
        end
    else
        p_nom = get(d, :p_nom, G)
        @variable(jm, p_min_pu[g,t] * p_nom[g] <= p[g=G,t=T] <= p_max_pu[g,t] * p_nom[g])
    end
end
