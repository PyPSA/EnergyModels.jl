struct EdgeInfo{T}
    name::Tuple{Symbol,T}
    src::Int64
end

graph(sn::SubNetwork; ctype=PassiveBranch) = graph(sn.model, ctype=ctype, buses=axis(sn, Bus))
function graph(m::EnergyModel; ctype=PassiveBranch, buses=axis(m, Bus))
    g = MetaGraph(length(buses))
    b = m[ctype]

    for (n, u, v) = zip(axis(b).val, indexin(b[:bus0], buses), indexin(b[:bus1], buses))
        if isnothing(u) || isnothing(v) continue end
        u = something(u)
        v = something(v)

        einfo = EdgeInfo(n, u)
        if ! add_edge!(g, u, v, :branches, [einfo])
            # edge already exists
            push!(get_prop(g, Edge(u, v), :branches), einfo)
        end
    end

    g
end

function determine_subnetworks!(m::EnergyModel)
    buses = axis(m, Bus)
    conn_comp = connected_components(graph(m, buses=buses))

    empty!(m.subnetworks)
    for (i,b) = enumerate(sort!(conn_comp, by=length, rev=true))
        push!(m, SubNetwork(m, Symbol(:subnetwork, i), Axis{axisname(buses)}(buses[b])))
    end
end

function cycle_matrix(sn::SubNetwork; ctype=PassiveBranch)
    ax = axis(sn, ctype)
    length(ax) == 0 && return sparse([], [], [])

    g = graph(sn, ctype=ctype)
    L = sum(length(get_prop(g, e, :branches)) for e = edges(g))

    # cycle_basis is only defined for a SimpleGraph (conversion seems to preserve order)
    cycles = cycle_basis(SimpleGraph(g))
    c = length(cycles) + 1 # counter for 2-edge cycles

    IJV = Tuple{Int64,Int64,Int64}[]
    for (i, cycle) = enumerate(cycles),
        j = eachindex(cycle)

        e = Edge(cycle[j], cycle[j % length(cycle) + 1])
        branches = get_prop(g, e, :branches)
        b1 = first(branches)
        i1 = findfirst(isequal(b1.name), ax.val)
        push!(IJV, (i1, i, b1.src == e.src ? +1 : -1))

        for b = branches[2:end]
            push!(IJV, (i1, c, 1))
            push!(IJV, (findfirst(isequal(b.name), ax.val), c, b.src == b1.src ? -1 : 1))
            c += 1
        end
    end

    sparse(destruct(IJV)..., ne(g), length(cycles) + L - ne(g))
end
