struct EdgeInfo{T}
    name::Tuple{Symbol,T}
    src::Int64
end

graph(sn::SubNetwork; ctype=PassiveBranch) = graph(sn.model, ctype=ctype, buses=axis(sn[Bus]))
function graph(m::EnergyModel; ctype=PassiveBranch, buses=axis(m[Bus]))
    g = MetaGraph(length(buses))
    b = m[ctype]

    for (n, u, v) = zip(axis(b), indexin(b[:bus0], buses), indexin(b[:bus1], buses))
        if u == 0 || v == 0 continue end

        einfo = EdgeInfo(n, u)
        if !add_edge!(g, u, v, :branches, [einfo])
            # edge already exists
            push!(get_prop(g, Edge(u, v), :branches), einfo)
        end
    end

    g
end

function determine_subnetworks!(m::EnergyModel)
    buses = axis(m[Bus])
    conn_comp = connected_components(graph(m, buses=buses))

    empty!(m.subnetworks)
    for b = sort!(conn_comp, by=length, rev=true)
        push!(m.subnetworks, SubNetwork(m, Axis{axisname(buses)}(buses[b])))
    end
end

function cycle_matrix(sn::SubNetwork; ctype=PassiveBranch)
    ax = axis(sn[ctype])
    length(ax) == 0 && return sparse([], [], [])

    g = graph(sn, ctype=ctype)
    L = sum(length(get_prop(g, e, :branches)) for e = edges(g))
    cycles = cycle_basis(g)
    c = length(cycles) + 1 # counter for 2-edge cycles

    IJV = Tuple{Int64,Int64,Int64}[]
    for (i, cycle) = enumerate(cycles),
        j = eachindex(cycle)

        e = Edge(cycle[j], cycle[j % length(cycle) + 1])
        branches = get_prop(g, e, :branches)
        b1 = first(branches)
        i1 = findfirst(ax, b1.name)
        push!(IJV, (i1, i, b1.src == e.src ? +1 : -1))

        for b = branches[2:end]
            push!(IJV, (i1, c, 1))
            push!(IJV, (findfirst(ax, b.name), c, b.src == b1.src ? -1 : 1))
            c += 1
        end
    end

    sparse(destruct(IJV)..., ne(g), length(cycles) + L - ne(g))
end
