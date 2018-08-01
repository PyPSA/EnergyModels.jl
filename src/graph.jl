struct EdgeInfo
    name::Symbol
    src::Int64
end

graph(sn::SubNetwork; ctype=PassiveBranchComponent) = graph(sn.model, ctype=ctype, buses=axis(sn, :buses))
function graph(m::EnergyModel; ctype=PassiveBranchComponent, buses=nothing)
    if buses === nothing
        buses = axis(m, :buses)
    end
    g = MetaGraph(length(buses))

    for c = components(m, ctype),
        (n, u, v) = zip(axis(c), indexin(c[:bus0], buses), indexin(c[:bus1], buses))
        if u == 0 || v == 0 continue end

        einfo = EdgeInfo(naming(Symbol, c, c.class, n), u)
        if !add_edge!(g, u, v, :branches, [einfo])
            # edge already exists
            push!(get_prop(g, Edge(u, v), :branches), einfo)
        end
    end

    g
end

function determine_subnetworks!(m::EnergyModel)
    conn_comp = connected_components(graph(m))
    for (i, b) = enumerate(sort!(conn_comp, by=length, rev=true))
        push!(m, SubNetwork(m, Symbol(string("sn", i)), b))
    end
end

function cycle_matrix(sn::SubNetwork; ctype=PassiveBranchComponent)
    ax = axis(sn, :branches, ctype=ctype)
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
        push!(IJV, (i1, j, b1.src == e.src ? +1 : -1))

        for b = branches[2:end]
            push!(IJV, (i1, c, 1))
            push!(IJV, (findfirst(ax, b.name), c, b.src == b1.src ? -1 : 1))
            c += 1
        end
    end

    sparse(destruct(IJV)...)
end
