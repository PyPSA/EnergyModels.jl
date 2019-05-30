using MacroTools: prewalk, @capture, @match

using JuMP: addtoexpr_reorder, esc_nonconstant, variable_error,
    constraint_error, getname, addkwargs!, constructvariable!, buildrefsets,
    isdependent, variabletype, getloopedcode, validmodel, coloncheck,
    EMPTYSTRING, JuMPContainer, registervar, storecontainerdata,
    _canonicalize_sense, registercon, parseExprToplevel, pushmeta!,
    addconstraint, constructconstraint!, addVectorizedConstraint

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
        reprs = (axes !== nothing && S !== nothing) ? maybe_escape.(map(s->axes[s], S)) : []
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
        if idxpair.idxset isa QuoteNode
            s = gensym()
            name = idxpair.idxset.value
            push!(initcode.args, :($s = axis($(esc(c)), $(idxpair.idxset))))
            idxpair.idxset = idxsets[i] = s
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

## Straight copy from JuMP master, will be removed as soon as we port

# Any fields can usually be either a number or an expression
mutable struct VariableInfoExpr
    haslb::Bool
    lowerbound::Any
    hasub::Bool
    upperbound::Any
    hasfix::Bool
    fixedvalue::Any
    hasstart::Bool
    start::Any
    binary::Any
    integer::Any
end

function setlowerbound_or_error(_error::Function, info::VariableInfoExpr, lower)
    info.haslb && _error("Cannot specify variable lowerbound twice")
    info.haslb = true
    info.lowerbound = lower
end
function setupperbound_or_error(_error::Function, info::VariableInfoExpr, upper)
    info.hasub && _error("Cannot specify variable lowerbound twice")
    info.hasub = true
    info.upperbound = upper
end
function fix_or_error(_error::Function, info::VariableInfoExpr, value)
    info.hasfix && _error("Cannot specify variable fixed value twice")
    info.hasfix = true
    info.fixedvalue = value
end
function setbinary_or_error(_error::Function, info::VariableInfoExpr)
    info.binary === false || _error("'Bin' and 'binary' keyword argument cannot both be specified.")
    info.binary = true
end
function setinteger_or_error(_error::Function, info::VariableInfoExpr)
    info.integer === false || _error("'Int' and 'integer' keyword argument cannot both be specified.")
    info.integer = true
end

function isinfokeyword(kw::Expr)
    kw.args[1] in [:lowerbound, :upperbound, :start, :binary, :integer]
end
# :(start = 0)     -> (:start, 0)
# :(start = i + 1) -> (:start, :($(Expr(:escape, :(i + 1)))))

function keywordify(kw::Expr)
    (kw.args[1], esc_nonconstant(kw.args[2]))
end

function VariableInfoExpr(; lowerbound=NaN, upperbound=NaN, start=NaN, binary=false, integer=false, fixedvalue=NaN)
    # isnan(::Expr) is not defined so we need to do !== NaN
    VariableInfoExpr(lowerbound !== NaN, lowerbound, upperbound !== NaN, upperbound, fixedvalue !== NaN, fixedvalue, start !== NaN, start, binary, integer)
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

    info_kwargs = filter(isinfokeyword, kwargs)
    extra_kwargs = filter(kw -> !isinfokeyword(kw), kwargs)
    infoexpr = VariableInfoExpr(; keywordify.(info_kwargs)...)

    ex = popfirst!(args)

    if isa(ex, Symbol)
        # fast-track for an unbounded singleton var
        @assert !in(ex, var_cats) "Ambiguous variable name $ex detected."
        var = ex
    else
        var = @match ex begin
            L_ <= V_ <= U_ => (setlowerbound_or_error(_error, infoexpr, L);
                               setupperbound_or_error(_error, infoexpr, U); V)
            V_ <= U_       => (setupperbound_or_error(_error, infoexpr, U); V)
            V_ >= L_       => (setlowerbound_or_error(_error, infoexpr, L); V)
            V_ == F_       => (fix_or_error(_error, infoexpr, F); V)
            V_             => V
        end

        # We only support named variables
        @assert isa(var, Symbol) || isexpr(var, :ref) "$(var) is not a valid variable"
    end

    t = quot(:Default)
    for ex in extra_kwargs
        if ex in var_cats
            t = ex
        end
    end

    ub = infoexpr.upperbound
    lb = infoexpr.lowerbound
    value = infoexpr.fixedvalue

    escvarname = esc(getname(var))
    quotvarname = :(naming($(esc(c)), $(quot(getname(var)))))

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

    quotvarname = :(naming($(esc(c)), $(quot(getname(v)))))
    escvarname  = esc(getname(v))

    if isa(x, Symbol)
        constraint_error(args, "Incomplete constraint specification $x. Are you missing a comparison (<=, >=, or ==)?")
    end

    (x.head == :block) &&
        constraint_error(args, "Code block passed as constraint. Perhaps you meant to use @emconstraints instead?")

    initcode = quote end
    m = extract_and_verify_model!(initcode, c)

    refcall, idxvars, idxsets, idxpairs, condition, axisreprs = embuildrefsets!(initcode, c, v, escvarname)

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

macro consense(x, msgs...)
    msg = isempty(msgs) ? string(x, " has conflicting values") : :(string($(esc(msgs[1])), ": ", unique(v)))
    quote
        v = $(esc(x))
        @assert(all(broadcast(==, v, first(v))), $msg)
        first(v)
    end
end
