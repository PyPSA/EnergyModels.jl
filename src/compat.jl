import AxisArrays: AxisArray, axisdim, axisnames, axisvalues, CategoricalVector
using JuMP: JuMPArray, addtoexpr, constructconstraint!
using LightGraphs: AbstractSimpleGraph
using Base: @propagate_inbounds, HasShape, HasEltype

const typeparsers = Dict("float"=>float, "string"=>identity, "int"=>i->parse(Int, i),
                         "bool"=>let d=Dict('t'=>true, 'f'=>false); i->d[first(i)] end)
const typenames = Dict("float"=>Float64, "string"=>String, "int"=>Int64, "bool"=>Bool)

# !!Type-piracy!! We should make the case to move these definitions to JuMP
# Will be difficult as long as they use their one JuMPArray methods!
axisnames(::JuMP.JuMPArray{T,N,Ax}) where {T,N,Ax}       = AxisArrays._axisnames(Ax)
axisnames(::Type{JuMP.JuMPArray{T,N,Ax}}) where {T,N,Ax} = AxisArrays._axisnames(Ax)
axisvalues(A::JuMP.JuMPArray) = axisvalues(A.indexsets...)

function axisdim(::Type{JuMPArray{T,N,Ax}}, ::Type{<:Axis{name}}) where {T,N,Ax,name}
    isa(name, Int) && return name <= N ? name : error("axis $name greater than array dimensionality $N")
    names = axisnames(Ax)
    idx = findfirst(names, name)
    idx == 0 && error("axis $name not found in array axes $names")
    idx
end
axisdim(A::JuMPArray, ax::Axis) = axisdim(A, typeof(ax))
@generated function axisdim(A::JuMPArray, ax::Type{Ax}) where Ax<:Axis
    dim = axisdim(A, Ax)
    :($dim)
end

AxisArray(A::JuMPArray) = AxisArray(A.innerArray, A.indexsets...)

function _shiftamt(A, shifts::Pair{Symbol,T}...) where T<:Integer
    amt = zeros(T, ndims(A))
    for (ax, s) = shifts
        amt[axisdim(A, Axis{ax})] += s
    end
    amt
end

Base.circshift(A::AxisArray, shifts::Pair{Symbol,<:Integer}...) = circshift(A, _shiftamt(A, shifts...))
function Base.circshift(A::JuMPArray, shifts::Pair{Symbol,<:Integer}...)
    B = JuMPArray(circshift(A.innerArray, _shiftamt(A, shifts...)), A.indexsets)
    merge!(B.meta, A.meta)
    B
end


Base.indexin(a::AbstractArray, b::Axis) = indexin(a, b.val)

# iteration on Axis
@inline Base.start(A::Axis) = 1
@propagate_inbounds Base.next(A::Axis, i) = (A[i], i+1)
@propagate_inbounds Base.done(A::Axis, i) = length(A) + 1 == i

# iteration traits
Base.iteratorsize(::Type{<:Axis}) = HasShape()
Base.iteratoreltype(::Type{<:Axis}) = HasEltype()

# vcat definition for categorical vector
Base.vcat(As::CategoricalVector...) = CategoricalVector(vcat(map(A -> A.data, As)...))

# This was submitted as a PR to LightGraphs.jl and will be included in future versions. https://github.com/JuliaGraphs/LightGraphs.jl/pull/929
# The LightGraph.jl version with the function only runs for Julia>v0.7. Therefore, the function is added for compatibility.

if VERSION < v"0.7"

    # Code in this function inspired by NetworkX.
    """
        cycle_basis(g, root=nothing)

    Return a list of cycles which form a basis for cycles of graph `g`, optionally starting at node `root`.

    A basis for cycles of a network is a minimal collection of
    cycles such that any cycle in the network can be written
    as a sum of cycles in the basis.  Here summation of cycles
    is defined as "exclusive or" of the edges. Cycle bases are
    useful, e.g. when deriving equations for electric circuits
    using Kirchhoff's Laws.

    Example:
    ```jldoctest
    julia> nlist = [1,2,3,4,5]
    julia> elist = [(1,2),(2,3),(2,4),(3,4),(4,1),(1,5)]
    julia> g = SimpleGraph(length(nlist))
    julia> for e in elist add_edge!(g, e) end
    julia> cycle_basis(g)
    2-element Array{Array{Int64,1},1}:
    [2, 3, 4]
    [2, 1, 3]
    ```

    ### References
    * Paton, K. An algorithm for finding a fundamental set of cycles of a graph. Comm. ACM 12, 9 (Sept 1969), 514-518. [https://dl.acm.org/citation.cfm?id=363232]
    """

    function cycle_basis(g::AbstractGraph, root=nothing)
        gnodes = Set(vertices(g))
        cycles = Vector{Vector{eltype(g)}}()
        while !isempty(gnodes)
            if root == nothing
                root = pop!(gnodes)
            end
            stack = [root]
            pred = Dict(root => root)
            keys_pred = Set(root)
            used = Dict(root => [])
            keys_used = Set(root)
            while !isempty(stack)
                z = pop!(stack)
                zused = used[z]
                for nbr in neighbors(g,z)
                    if !in(nbr, keys_used)
                        pred[nbr] = z
                        push!(keys_pred, nbr)
                        push!(stack,nbr)
                        used[nbr] = [z]
                        push!(keys_used, nbr)
                    elseif nbr == z
                        push!(cycles, [z])
                    elseif !in(nbr, zused)
                        pn = used[nbr]
                        cycle = [nbr,z]
                        p = pred[z]
                        while !in(p, pn)
                            push!(cycle, p)
                            p = pred[p]
                        end
                        push!(cycle,p)
                        push!(cycles,cycle)
                        push!(used[nbr], z)
                    end
                end
            end
            setdiff!(gnodes,keys_pred)
            root = nothing
        end
        return cycles
    end

end

function JuMP.constructvariable!(m::Model, _error::Function, lowerbound::Number, upperbound::AffExpr, args...; kwargs...)
    v = constructvariable!(m, _error, lowerbound, Inf, args...; kwargs...)
    addconstraint(m, constructconstraint!(addtoexpr(upperbound, -1.0, v), :(>=)))
    v
end

function JuMP.constructvariable!(m::Model, _error::Function, lowerbound::AffExpr, upperbound::Number, args...; kwargs...)
    v = constructvariable!(m, _error, -Inf, upperbound, args...; kwargs...)
    addconstraint(m, constructconstraint!(addtoexpr(lowerbound, -1.0, v), :(<=)))
    v
end

function JuMP.constructvariable!(m::Model, _error::Function, lowerbound::AffExpr, upperbound::AffExpr, args...; kwargs...)
    v = constructvariable!(m, _error, -Inf, Inf, args...; kwargs...)
    addconstraint(m, constructconstraint!(addtoexpr(lowerbound, -1.0, v), :(<=)))
    addconstraint(m, constructconstraint!(addtoexpr(upperbound, -1.0, v), :(>=)))
    v
end
