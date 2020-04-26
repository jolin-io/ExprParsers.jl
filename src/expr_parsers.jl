using Base.Iterators
using SimpleMatch
using StructEquality
using ProxyInterface


"""
All ExprParser have a common parse_expr method, namely that all struct fields are given directly as keyword arguments.
"""
function parse_expr(parser::ExprParser; kw...)
  names = kw.itr  # kw isa Base.Iterators.Pairs
  matches = [parse_expr(getproperty(parser, name), value) for (name, value) in kw]
  kw′ = NamedTuple{names}(matches)
  ExprParsed(typeof(parser))(;kw′...)
end
function parse_expr(parser::ExprParser, any)
  throw(ParseError("$(typeof(parser)) has no clause defined to capture Type '$(typeof(any))'. Got: $any"))
end

# ExprParsers
# ===========

# Expr
# ----

"""
parses:
```
Base.Expr(head, args...)
```
"""
@exprparser struct Expr
  head = anything
  args = anything
end
function Expr(expr::Base.Expr; ignore_linenumbernodes=true)
  if ignore_linenumbernodes
    Expr(expr.head, Block(expr.args..., ignore_linenumbernodes=true))
  else
    Expr(expr.head, expr.args)
  end
end
parse_expr(p::Expr, expr::Base.Expr) = parse_expr(p, head = expr.head, args = expr.args)
function to_expr(pp::Expr_Parsed)
  if pp.args isa Block_Parsed
    Base.Expr(to_expr(pp.head), to_expr(pp.args.exprs)...)
  else
    Base.Expr(to_expr(pp.head), to_expr(pp.args)...)
  end
end


# Symbol
# ------
"""
parses only symbols
"""
@exprparser struct Symbol
  symbol = anything
end

parse_expr(p::Symbol, symbol::Base.Symbol) = parse_expr(p, symbol = symbol)
parse_expr(p::Symbol, other) = throw(ParseError("Symbol only parses Symbols, got $other"))
to_expr(pp::Symbol_Parsed) = to_expr(pp.symbol)


# Block
# -----

"""
parses standard blocks of code or Vectors of Base.Expr
"""
@def_structequal struct Block <: ExprParser
  exprs
  ignore_linenumbernodes::Bool

  function Block(expr::Expr; ignore_linenumbernodes = true)
    @assert expr.head == :block "Trying to construct a Block parser from a $(expr.head). Expecting :block."
    new(expr.args, ignore_linenumbernodes)
  end
  Block(exprs...; ignore_linenumbernodes=true) = new(exprs, ignore_linenumbernodes)
  Block() = new(Iterator(repeated(anything)), true)
end
@def_structequal mutable struct Block_Parsed <: ExprParsed
  exprs
end
ExprParsed(::Base.Type{Block}) = Block_Parsed
to_expr(bpp::Block_Parsed) = Base.Expr(:block, map(to_expr, bpp.exprs)...)

function parse_expr(bp::Block, expr::Base.Expr)
  @passert expr.head == :block
  parse_expr(bp, expr.args)
end
function parse_expr(bp::Block, exprs::Union{Vector, Tuple})
  parsed_expr = if bp.ignore_linenumbernodes
    parseexpr_ignore_LineNumberNodes(bp.exprs, exprs)
  else
    parse_expr(bp.exprs, exprs)
  end
  Block_Parsed(parsed_expr)
end

parseexpr_ignore_LineNumberNodes(a, b) = parse_expr(a, b)
function parseexpr_ignore_LineNumberNodes(args1::Union{Vector, Tuple, Iterator}, args2::Tuple)
  parseexpr_ignore_LineNumberNodes(args1, collect(args2))
end
function parseexpr_ignore_LineNumberNodes(args1::Union{Vector, Tuple, Iterator}, args2::Vector)
  args_parsed = similar(args2)
  args1_filtered = Iterators.filter(x -> !isa(x, LineNumberNode), args1)
  next1 = iterate(args1_filtered)
  for (i, a2) in enumerate(args2)
    if a2 isa LineNumberNode
      args_parsed[i] = a2
    else
      @passert issomething(next1) "found element `$a2` in args2 $args2 without a match in args1 $args1."
      a1, a1_state = next1
      args_parsed[i] = parseexpr_ignore_LineNumberNodes(a1, a2)
      next1 = iterate(args1_filtered, a1_state)
    end
  end
  @passert isnothing(next1) "While args2 $args2 is exhausted, there is still a rest $next1 in args1 $args1"
  args_parsed
end
function parseexpr_ignore_LineNumberNodes(expr1::Expr, expr2::Expr)
  @passert expr1.head == expr2.head
  Base.Expr(expr1.head, parseexpr_ignore_LineNumberNodes(expr1.args, expr2.args)...)
end


# Macro
# -----

"""
parses:
```
@macroname arg1 arg2 ...
```
"""
@exprparser struct Macro
  name = anything
  args = anything
  linenumber = Isa(LineNumberNode)
end
to_expr(mpp::Macro_Parsed) = Base.Expr(:macrocall, Base.Symbol("@", mpp.name), mpp.linenumber, mpp.args...)
function parse_expr(mp::Macro, expr::Base.Expr)
  @passert expr.head == :macrocall
  name_with_at = expr.args[1]
  name_without_at = Base.Symbol(string(name_with_at)[2:end])
  parse_expr(mp, name = name_without_at, args = expr.args[3:end], linenumber = expr.args[2])
end


# Assignment
# ----------

"""
parses:
```
left = right
```
"""
@exprparser struct Assignment
  left = anything
  right = anything
end
to_expr(app::Assignment_Parsed) = :($(app.left) = $(app.right))
function parse_expr(ap::Assignment, expr::Base.Expr)
  @passert expr.head == :(=)  "assignment head not found"
  parse_expr(ap, left = expr.args[1], right = expr.args[2])
end

# NestedDot
# ---------

"""
parses:
```
a.b
fun(T{:hi}).b.c.d.e.f
```
"""
@exprparser struct NestedDot
  base = anything
  properties = anything
end
function parse_expr(ndp::NestedDot, expr::Base.Expr)
  @passert expr.head == :.
  exprs = _collect_nested_dots(expr)
  parse_expr(ndp, base = exprs[1], properties = exprs[2:end])
end
_collect_nested_dots(expr::Base.Expr) = _collect_nested_dots(expr, Val{expr.head}())
function _collect_nested_dots(expr::Base.Expr, ::Val{:.})
  @passert length(expr.args) == 2
  @passert expr.args[2] isa QuoteNode
  [_collect_nested_dots(expr.args[1]); expr.args[2].value]
end
_collect_nested_dots(expr::Base.Expr, _::Val) = [expr]
_collect_nested_dots(any) = [any]

to_expr(ndpp::NestedDot_Parsed) = foldl(ndpp.properties; init=ndpp.base) do a, b
  Base.Expr(:., a, QuoteNode(b))
end


# Reference
# ---------

"""
parses:
```
a
a{b, c}
```
"""
@exprparser struct Reference
  name = anything
  curlies = anything
end

parse_expr(p::Reference, e::Base.Symbol) = parse_expr(p, name = e, curlies = [])
function parse_expr(p::Reference, e::Base.Expr)
  if e.head == :curly
    parse_expr(p, name = e.args[1], curlies = e.args[2:end])
  elseif e.head == :.
    parse_expr(p, name = e, curlies = [])
  else
    throw(ParseError("Cannot parse expression as reference: $e"))
  end
end

function to_expr(pp::Reference_Parsed)
  if isempty(pp.curlies)
    to_expr(pp.name)
  else
    :( $(to_expr(pp.name)){$(to_expr(pp.curlies)...)} )
  end
end


# Call
# ----

_is_kws(expr::Base.Expr) = expr.head == :parameters
_is_kws(any) = false

"""
parses:
```
a()
a(b, c)
a{b, c}(d, e)
```
"""
@exprparser struct Call
  name = anything
  curlies = anything
  args = anything
  kwargs = anything
end

function parse_expr(p::Call, e::Base.Expr)
  @passert e.head == :call
  called = parse_expr(Reference(), e.args[1])
  args, kwargs = if _is_kws(e.args[2])
    e.args[3:end], e.args[2].args
  else
    e.args[2:end], []
  end
  parse_expr(p, name = called.name, curlies = called.curlies, args = args, kwargs = kwargs)
end

to_expr(pp::Call_Parsed) = toAST_Call(pp, Val{nonempty(pp.curlies)}(), Val{nonempty(pp.kwargs)}())
toAST_Call(pp, iscurlies::True, iswheres::True) = :(
  $(to_expr(pp.name)){$(to_expr(pp.curlies)...)}($(to_expr(pp.args)...),;$(to_expr(pp.kwargs)...))
)
toAST_Call(pp, iscurlies::True, iswheres::False) = :(
  $(to_expr(pp.name)){$(to_expr(pp.curlies)...)}($(to_expr(pp.args)...),)
)
toAST_Call(pp, iscurlies::False, iswheres::True) = :(
  $(to_expr(pp.name))($(to_expr(pp.args)...),;$(to_expr(pp.kwargs)...))
)
toAST_Call(pp, iscurlies::False, iswheres::False) = :(
  $(to_expr(pp.name))($(to_expr(pp.args)...),)
)


# Signature
# ---------

"""
parses:
```
a(b, c::Any)
a(b::B, c) where B
(::Any, c::C) where {C <: Number}
```
"""
@exprparser struct Signature
  name = anything
  curlies = anything
  args = anything
  kwargs = anything
  wheres = anything
end

function parse_expr(p::Signature, expr::Base.Expr)
  @passert expr.head in (:where, :call, :tuple)
  # we don't reuse ParsedCallExpr because functions-signatures can actually be anonymous
  call, wheres = split_where(expr)
  @passert call isa Base.Expr  # until before here we still match assignments
  @passert call.head in (:tuple, :call)
  isanonymous = call.head == :tuple

  name, curlies, args, kwargs = if isanonymous
    if _is_kws(call.args[1])
      nothing, [], call.args[2:end], call.args[1].args
    else
      nothing, [], call.args, []
    end
  else
    parsed = parse_expr(Call(), call)
    parsed.name, parsed.curlies, parsed.args, parsed.kwargs
  end
  parse_expr(p, name = name, curlies = curlies, args = args, kwargs = kwargs, wheres = wheres)
end

to_expr(pp::Signature_Parsed) = _toAST_Signature(pp, pp.name, Val{nonempty(pp.curlies)}(), Val{nonempty(pp.kwargs)}(), Val{nonempty(pp.wheres)}())
_toAST_Signature(pp, ::Nothing, iscurlies::True, _, _) = error("Impossible Reached: Given curlies but no name. pp.curlies = $(pp.curlies)")
_toAST_Signature(pp, ::Nothing, ::False, iskwargs::False, iswheres::False) = :(
  ($(to_expr(pp.args)...),)
)
_toAST_Signature(pp, ::Nothing, ::False, iskwargs::True, iswheres::False) = :(
  ($(to_expr(pp.args)...), ; $(to_expr(pp.kwargs)...))
)
_toAST_Signature(pp, ::Nothing, ::False, iskwargs::False, iswheres::True) = :(
  ($(to_expr(pp.args)...),) where {$(to_expr(pp.wheres)...)}
)
_toAST_Signature(pp, ::Nothing, ::False, iskwargs::True, iswheres::True) = :(
  ($(to_expr(pp.args)...),; $(to_expr(pp.kwargs)...)) where {$(to_expr(pp.wheres)...)}
)
_toAST_Signature(pp, name, iscurlies::True, iskwargs::True, iswheres::True) = :(
  $(to_expr(pp.name)){$(to_expr(pp.curlies)...)}($(to_expr(pp.args)...), ; $(to_expr(pp.kwargs)...)) where {$(to_expr(pp.wheres)...)}
)
_toAST_Signature(pp, name, iscurlies::True, iskwargs::True, iswheres::False) = :(
  $(to_expr(pp.name)){$(to_expr(pp.curlies)...)}($(to_expr(pp.args)...), ; $(to_expr(pp.kwargs)...))
)
_toAST_Signature(pp, name, iscurlies::True, iskwargs::False, iswheres::True) = :(
  $(to_expr(pp.name)){$(to_expr(pp.curlies)...)}($(to_expr(pp.args)...),) where {$(to_expr(pp.wheres)...)}
)
_toAST_Signature(pp, name, iscurlies::True, iskwargs::False, iswheres::False) = :(
  $(to_expr(pp.name)){$(to_expr(pp.curlies)...)}($(to_expr(pp.args)...),)
)
_toAST_Signature(pp, name, iscurlies::False, iskwargs::True, iswheres::True) = :(
  $(to_expr(pp.name))($(to_expr(pp.args)...), ; $(to_expr(pp.kwargs)...)) where {$(to_expr(pp.wheres)...)}
)
_toAST_Signature(pp, name, iscurlies::False, iskwargs::True, iswheres::False) = :(
  $(to_expr(pp.name))($(to_expr(pp.args)...), ; $(to_expr(pp.kwargs)...))
)
_toAST_Signature(pp, name, iscurlies::False, iskwargs::False, iswheres::True) = :(
  $(to_expr(pp.name))($(to_expr(pp.args)...),) where {$(to_expr(pp.wheres)...)}
)
_toAST_Signature(pp, name, iscurlies::False, iskwargs::False, iswheres::False) = :(
  $(to_expr(pp.name))($(to_expr(pp.args)...),)
)

# Function
# --------

"""
parses:
```
function a(b, c) where B
  d
end
a(b, c) = d
```
"""
@exprparser struct Function
  name = anything
  curlies = anything
  args = anything
  kwargs = anything
  wheres = anything
  body = anything
end

function parse_expr(p::Function, expr::Base.Expr)
  @passert expr.head in (:function, :(=))
  @passert length(expr.args) == 2
  body = expr.args[2]

  signature = parse_expr(Signature(), expr.args[1])
  parse_expr(p,
    name = signature.name,
    curlies = signature.curlies,
    args = signature.args,
    kwargs = signature.kwargs,
    wheres = signature.wheres,
    body = body)
end

to_expr(pp::Function_Parsed) = Base.Expr(:function, to_expr(Signature_Parsed(pp)), to_expr(pp.body))

Signature_Parsed(pp::Function_Parsed) = Signature_Parsed(
  name = pp.name,
  curlies = pp.curlies,
  args = pp.args,
  kwargs = pp.kwargs,
  wheres = pp.wheres,
)

# Type
# ----

"""
parses:
```
a
a{b, c}
a{d} where d
```
"""
@exprparser struct Type
  name = anything
  curlies = anything
  wheres = anything
end

parse_expr(p::Type, e::Base.Symbol) = parse_expr(p, name = e, curlies = [], wheres = [])
parse_expr(p::Type, e::Base.Type) = parse_expr(p, name = e, curlies = [], wheres = [])
function parse_expr(p::Type, e::Base.Expr)
  e′, wheres = split_where(e)
  ref = parse_expr(Reference(), e′)
  parse_expr(p, name = ref.name, curlies = ref.curlies, wheres = wheres)
end

to_expr(pp::Type_Parsed) = toAST_Type(pp, Val{nonempty(pp.curlies)}(), Val{nonempty(pp.wheres)}())
toAST_Type(pp, curlies::True, wheres::True) = :(
  $(to_expr(pp.name)){$(to_expr(pp.curlies)...)} where {$(to_expr(pp.wheres)...)}
)
toAST_Type(pp, curlies::True, wheres::False) = :(
  $(to_expr(pp.name)){$(to_expr(pp.curlies)...)}
)
toAST_Type(pp, curlies::False, wheres::True) = :(
  $(to_expr(pp.name)) where {$(to_expr(pp.wheres))}
)
toAST_Type(pp, curlies::False, wheres::False) = to_expr(pp.name)


# TypeRange
# ---------

"""
parses:
```
TypeVar >: LowerBound
TypeVar <: UpperBound
LowerBound <: TypeVar <: UpperBound
```

Note: Construct with typevar = Parsers.Symbol() to guarantee that only plain symbols can be used as type variable
"""
@exprparser struct TypeRange
  # naming is analog to Base.TypeVar
  lb = anything
  name = anything
  ub = anything
end

function parse_expr(trp::TypeRange, expr::Base.Expr)
  if expr.head == :>:
    parse_expr(trp, lb = expr.args[2], name = expr.args[1], ub = Any)
  elseif expr.head == :<:
    parse_expr(trp, lb = Union{}, name = expr.args[1], ub = expr.args[2])
  elseif expr.head == :comparison && length(expr.args) == 5 && expr.args[2] == expr.args[4] == :<:
    parse_expr(trp, lb = expr.args[1], name = expr.args[3], ub = expr.args[5])
  else
    throw(ParseError("TypeRange only parses <:, >: constructs, but got: $expr"))
  end
end

to_expr(trpp::TypeRange_Parsed) = toAST_TypeRange(trpp.lb, trpp.name, trpp.ub)
toAST_TypeRange(::Base.Type{Union{}}, name, ::Base.Type{Any}) = name
toAST_TypeRange(lb, name, ::Base.Type{Any}) = :($name >: $lb)
toAST_TypeRange(::Base.Type{Union{}}, name, ub) = :($name <: $ub)
toAST_TypeRange(lb, name, ub) = :($lb <: $name <: $ub)


# TypeAnnotation
# --------------

"""
parses:
```
a
a::B
::B
```
"""
@exprparser struct TypeAnnotation
  name = anything
  type = anything
end

function parse_expr(p::TypeAnnotation, e::Base.Expr)
  @passert e.head == :(::)
  @passert length(e.args) <= 2
  if length(e.args) == 1
    parse_expr(p, name = nothing, type = e.args[1])
  else
    parse_expr(p, name = e.args[1], type = e.args[2])
  end
end
function to_expr(pp::TypeAnnotation_Parsed)
  if isnothing(pp.name)
    :(::$(to_expr(pp.type)))
  else
    :($(to_expr(pp.name))::$(to_expr(pp.type)))
  end
end


# Arg
# ---

struct NoDefault end
const nodefault = NoDefault()

"""
parses:
```
a
a::B
::B
a = c
a::B = c
```
"""
@exprparser struct Arg
  # name == nothing will be treated as anonymous type annotation
  name = anything
  type = anything
  default = anything
end

function parse_expr(p::Arg, e::Base.Symbol)
  # don't use the parser for ``default``
  Arg_Parsed(
    name = parse_expr(p.name, e),
    type = parse_expr(p.type, Any),
    default = nodefault)
end

const _arg_left_handside_parser = AnyOf(Symbol(), TypeAnnotation())
function parse_expr(p::Arg, expr::Base.Expr)
  if expr.head == :kw
    name, type = @match(parse_expr(_arg_left_handside_parser, expr.args[1])) do f
      f(s::Symbol_Parsed) = s.symbol, Any
      function f(a::TypeAnnotation_Parsed)
        @passert issomething(a.name) "need variable name if default is given"
        a.name, a.type
      end
    end
    parse_expr(p, name = name, type = type, default = expr.args[2])
  elseif expr.head == :...
    # Vararg is not a valid Type https://github.com/JuliaLang/julia/issues/30995
    parse_expr(p, name = expr.args[1], type = Vararg, default = nodefault)
  else
    name, type = @match(parse_expr(_arg_left_handside_parser, expr)) do f
      f(s::Symbol_Parsed) = s.symbol, Any
      f(a::TypeAnnotation_Parsed) = a.name, a.type
    end
    # don't use the parser for ``default``
    Arg_Parsed(
      name = parse_expr(p.name, name),
      type = parse_expr(p.type, type),
      default = nodefault)
  end
end

to_expr(pp::Arg_Parsed) = _toAST_Arg(pp.name, pp.type, pp.default)
_toAST_Arg(::Nothing, type, ::NoDefault) = :(::$(to_expr(type)))
_toAST_Arg(name, ::Base.Type{Any}, ::NoDefault) = to_expr(name)
function _toAST_Arg(name, type, ::NoDefault)
  if type === Vararg
    Base.Expr(:..., to_expr(name))
  else
    :($(to_expr(name))::$(to_expr(type)))
  end
end
_toAST_Arg(name, ::Base.Type{Any}, default) = Base.Expr(:kw, to_expr(name), to_expr(default))
_toAST_Arg(name, type, default) = Base.Expr(:kw, :($(to_expr(name))::$(to_expr(type))), to_expr(default))
