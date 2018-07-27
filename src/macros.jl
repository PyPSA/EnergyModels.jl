using MacroTools: prewalk, @capture, @match

using JuMP: esc_nonconstant, variable_error, constraint_error, getname, addkwargs!,
    constructvariable!, buildrefsets, isdependent, variabletype, getloopedcode,
    validmodel, coloncheck, EMPTYSTRING, JuMPContainer, registervar, JuMPArray,
    storecontainerdata, _canonicalize_sense, registercon, parseExprToplevel, pushmeta!

using NamedTuples

using Base.Meta: isexpr, quot

const var_cats = [:Cont, :Int, :Bin, :SemiCont, :SemiInt]

"""
    extract_and_verify_model(c)
"""
function extract_and_verify_model!(code, c)
    @assert isexpr(code, :block)

    m = gensym()
    c = esc(c)

    push!(code.args, :($m = jumpmodel($c)))
    push!(code.args, :(validmodel($m, string("jumpmodel(", $c, ")"))))

    m
end

"""
    extract_component_vars!(code, c, ex, indices=nothing)

Look for c[_Symbol] and c[_Symbol][__] and pull out the represented data into a
view in the expression block in `code`.
"""
function extract_component_vars!(code, c, ex, axes=nothing)
    @assert isexpr(code, :block)

    function pullout(T, S)
        v = gensym()
        reprs = (axes !== nothing && S !== nothing) ? maybe_escape.(getindex.(axes, S)) : []
        push!(code.args, :($(esc(v)) = view($(esc(c)), $T, ($(reprs...),))))
        S === nothing ? v : Expr(:ref, v, S...)
    end

    prewalk(x -> @capture(x, c[T_][S__] | c[T_]) ? pullout(T, S) : x, ex)
end

struct AxisRepr
    axis::Union{Symbol,Expr}
    name::Union{Nothing,Symbol}
end

maybe_escape(idx::AxisRepr) = idx.name === nothing ? esc(idx.axis) : idx.axis

function embuildrefsets!(initcode, c, var, escvarname)
    refcall, idxvars, idxsets, idxpairs, condition = buildrefsets(var, escvarname)

    axisreprs = Dict{Symbol,AxisRepr}()
    for (i, idxpair) = enumerate(idxpairs)
        if isexpr(idxpair.idxset, :quote)
            s = gensym()
            name = idxpair.idxset.args[1]
            idxpair.idxset = idxsets[i] = s
            push!(initcode.args, :($s = axis($(esc(c)), $(quot(name)))))

            axisrepr = AxisRepr(s, name)
        else
            axisrepr = AxisRepr(idxpair.idxset, nothing)
        end

        if idxpair.idxvar !== nothing
            axisreprs[idxpair.idxvar] = axisrepr
        end
    end

    refcall, idxvars, idxsets, idxpairs, condition, axisreprs
end

"""
    @emvariable(c, args...)

Construct a new scalar or vectorized variable in a similar manner to the
`@variable` JuMP macro.

Additionally:

* occurences of `c[symbol]` in the bounds are replaced with views into the
  component parameters

* if the indexing sets are given as a symbol as in the example above, they are
  resolved from the component data using `axis(c, symb)`

Example:
```julia
@emvariable c c[:p_nom_min][g] <= p_nom[g=:generators] <= c[:p_nom_max][g]
```
"""
macro emvariable(c, args...)
    _error(str) = variable_error(args, str)
    # _error(str) = macro_error(:variable, args, str)

    args = collect(args)

    kwargs = filter(x->isexpr(x, :(=)), args)
    args = filter(x->!isexpr(x, :(=)), args)

    ex = shift!(args)

    if isa(ex, Symbol)
        # fast-track for an unbounded singleton var
        @assert !in(ex, var_cats) "Ambiguous variable name $ex detected."
        p = @NT(var=ex)
    else
        p = @match ex begin
            L_ <= V_ <= U_ => @NT(var=V, lb=L, ub=U)
            V_ <= U_       => @NT(var=V, ub=U)
            V_ >= L_       => @NT(var=V, lb=L)
            V_ == F_       => @NT(var=V, t=:Fixed, value=F, ub=F, lb=F)
            V_             => @NT(var=V)
        end

        # We only support named variables
        @assert isa(p.var, Symbol) || isexpr(p.var, :ref) "$(p.var) is not a valid variable"
    end

    for ex in args
        if ex in var_cats
            @assert !haskey(p, :t) "Specified categories $(p.t) and $(ex)"
            parse = setindex(parse, :t, ex)
        end
    end

    value = NaN
    extra_kwargs = []
    for ex in kwargs
        kwarg = ex.args[1]
        if kwarg == :start
            value = esc(ex.args[2])
        elseif kwarg == :lowerbound
            @assert !haskey(p, :lb) "Cannot specify variable lowerbound twice"
            p = setindex(p, :lb, ex.args[2])
        elseif kwarg == :upperbound
            @assert !haskey(p, :ub) "Cannot specify variable lowerbound twice"
            p = setindex(p, :ub, ex.args[2])
        elseif kwarg == :category
            @assert !haskey(p, :t) "Cannot specify variable category twice"
            p = setindex(p, :t, ex.args[2])
        elseif in(kwarg, (:objective, :inconstraints, :coefficients))
            error("No support for column generation in @emvariable, yet.")
        elseif kwarg == :basename
            error("The anonymous syntax is not supported in @emvariable.")
        else
            push!(extra_kwargs, ex)
        end
    end

    var = p.var
    ub = get(p, :ub, Inf)
    lb = get(p, :lb, -Inf)
    t = quot(get(p, :t, :Default))
    value = get(p, :value, NaN)

    escvarname = esc(getname(var))
    quotvarname = :(naming(Symbol, $(esc(c)), $(esc(c)).class, $(quot(getname(var)))))

    initcode = Expr(:block)
    m = extract_and_verify_model!(initcode, c)

    if isa(var, Symbol)
        # Easy case - a single variable

        # Extract vars from upper and lower bounds
        ub = esc_nonconstant(extract_component_vars!(initcode, c, ub))
        lb = esc_nonconstant(extract_component_vars!(initcode, c, lb))

        variablecall = :( constructvariable!($m, $(args...), $_error, $lb, $ub, $t, string($quotvarname), $value) )
        addkwargs!(variablecall, extra_kwargs)
        quote
            $initcode
            $escvarname = $variablecall
            registervar($m, $quotvarname, $escvarname)
        end
    elseif isa(var, Expr)
        # We now build the code to generate the variables (and possibly the JuMPDict
        # to contain them)
        refcall, idxvars, idxsets, idxpairs, condition, axisreprs = embuildrefsets!(initcode, c, var, escvarname)

        ub = esc_nonconstant(extract_component_vars!(initcode, c, ub, axisreprs))
        lb = esc_nonconstant(extract_component_vars!(initcode, c, lb, axisreprs))

        # Code to be used to create each variable of the container.
        variablecall = :( constructvariable!($m, $(args...), $_error, $lb, $ub, $t, EMPTYSTRING, $value) )
        addkwargs!(variablecall, extra_kwargs)
        vartype = :( variabletype($m, $(args...)) )
        code = quote
            $refcall = $variablecall
            coloncheck($(refcall.args[2:end]...))
        end
        # Determine the return type of constructvariable!. This is needed to create the container holding them.

        loopedcode = getloopedcode(escvarname, code, condition, idxvars, idxsets, idxpairs, vartype)

        clear_dependencies(i) = (isdependent(idxvars,idxsets[i],i) ? () : idxsets[i])
        independents = Expr(:tuple, map(clear_dependencies, 1:length(idxsets))...)

        quote
            $initcode
            $loopedcode
            isa($escvarname, JuMPContainer) && pushmeta!($escvarname, :model, $m)
            push!($(m).dictList, $escvarname)
            registervar($m, $quotvarname, $escvarname)
            storecontainerdata($m, $escvarname, $quotvarname, $independents,
                               $idxpairs, $(quot(condition)))
        end
    end

end



macro emconstraint(c, args...)
    # Pick out keyword arguments
    args = collect(args)

    kwargs = filter(x->isexpr(x, :(=)), args)

    # Contrary to the original constraint macro we do not support anonymous
    # constraints
    # @emconstraint(c, myref[a=1:5], a*x <= 5)

    v, x = filter(x->!isexpr(x, :(=)), args)

    quotvarname = :(naming(Symbol, $(esc(c)), $(esc(c)).class, $(quot(getname(v)))))
    escvarname  = esc(getname(v))

    if isa(x, Symbol)
        constraint_error(args, "Incomplete constraint specification $x. Are you missing a comparison (<=, >=, or ==)?")
    end

    (x.head == :block) &&
        constraint_error(args, "Code block passed as constraint. Perhaps you meant to use @emconstraints instead?")

    initcode = quote end
    m = extract_and_verify_model!(initcode, c)

    # Strategy: build up the code for non-macro addconstraint, and if needed
    # we will wrap in loops to assign to the ConstraintRefs
    refcall, idxvars, idxsets, idxpairs, condition, axisreprs = embuildrefsets!(initcode, c, v, escvarname)

    # JuMP accepts constraint syntax of the form @constraint(m, foo in bar).
    # This will be rewritten to a call to constructconstraint!(foo, bar). To
    # extend JuMP to accept set-based constraints of this form, it is necessary
    # to add the corresponding methods to constructconstraint!. Note that this
    # will likely mean that bar will be some custom type, rather than e.g. a
    # Symbol, since we will likely want to dispatch on the type of the set
    # appearing in the constraint.
    if isexpr(x, :call)
        if x.args[1] == :in
            constraint_error("Iteration over in is not supported with emconstraint, for now.")
            # @assert length(x.args) == 3
            # newaff, parsecode = parseExprToplevel(x.args[2], :q)
            # constraintcall = :(addconstraint($m, constructconstraint!($newaff,$(esc(x.args[3])))))
        end

        # Simple comparison - move everything to the LHS
        @assert length(x.args) == 3
        (sense,vectorized) = _canonicalize_sense(x.args[1])
        addconstr = (vectorized ? :addVectorizedConstraint : :addconstraint)

        lhs = extract_component_vars!(initcode, c, :($(x.args[2]) - $(x.args[3])), axisreprs)
        newaff, parsecode = parseExprToplevel(lhs, :q)
        constraintcall = :($addconstr($m, constructconstraint!($newaff,$(quot(sense)))))
        addkwargs!(constraintcall, kwargs)
        code = quote
            q = zero(AffExpr)
            $parsecode
            $(refcall) = $constraintcall
        end
    elseif isexpr(x, :comparison)
        # Ranged row
        (lsign,lvectorized) = _canonicalize_sense(x.args[2])
        (rsign,rvectorized) = _canonicalize_sense(x.args[4])
        if (lsign != :(<=)) || (rsign != :(<=))
            constraint_error(args, "Only ranged rows of the form lb <= expr <= ub are supported.")
        end
        ((vectorized = lvectorized) == rvectorized) || constraint_error("Signs are inconsistently vectorized")
        addconstr = (lvectorized ? :addVectorizedConstraint : :addconstraint)
        x_str = string(x)
        lb_str = string(x.args[1])
        ub_str = string(x.args[5])

        aff = extract_component_vars!(initcode, c, x.args[3], axisreprs)
        lb  = extract_component_vars!(initcode, c, x.args[1], axisreprs)
        ub  = extract_component_vars!(initcode, c, x.args[5], axisreprs)

        newaff, parsecode = parseExprToplevel(aff, :aff)
        newlb, parselb = parseExprToplevel(lb, :lb)
        newub, parseub = parseExprToplevel(ub, :ub)

        constraintcall = :($addconstr($m, constructconstraint!($newaff,$newlb,$newub)))
        addkwargs!(constraintcall, kwargs)
        code = quote
            aff = zero(AffExpr)
            $parsecode
            lb = 0.0
            $parselb
            ub = 0.0
            $parseub
        end
        if vectorized
            code = quote
                $code
                lbval, ubval = $newlb, $newub
            end
        else
            code = quote
                $code
                CoefType = coeftype($newaff)
                lbval = convert(CoefType, $newlb)
                ubval = convert(CoefType, $newub)
            end
        end
        code = quote
            $code
            $(refcall) = $constraintcall
        end
    else
        # Unknown
        constraint_error(args, string("Constraints must be in one of the following forms:\n" *
              "       expr1 <= expr2\n" * "       expr1 >= expr2\n" *
              "       expr1 == expr2\n" * "       lb <= expr <= ub"))
    end

    loopedcode = getloopedcode(escvarname, code, condition, idxvars, idxsets, idxpairs, :ConstraintRef)

    return quote
        $initcode
        $loopedcode
        registercon($m, $quotvarname, $escvarname)
    end
end
