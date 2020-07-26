using Base.Iterators
using SimpleMatch
using StructEquality
using ProxyInterface


"""
All ExprParserWithParsed have a common parse_expr method, namely that all struct fields are given directly as keyword arguments.
"""
function parse_expr(parser::ExprParserWithParsed; kw...)
  names = kw.itr  # kw isa Base.Iterators.Pairs
  matches = [parse_expr(getproperty(parser, name), value) for (name, value) in kw]
  kw′ = NamedTuple{names}(matches)
  ExprParsed(typeof(parser))(;kw′...)
end
function parse_expr(parser::ExprParserWithParsed, any)
  throw(ParseError("$(typeof(parser)) has no clause defined to capture Type '$(typeof(any))'. Got: $any"))
end

# one-line version
function Base.show(io::IO, obj::Union{ExprParserWithParsed, ExprParsed})
  type_name = typeof(obj).name.name
  print(io, "EP.$type_name(")
  field_names = fieldnames(typeof(obj))
  for field in field_names[1:end-1]
    print(io, "$field=$(repr(getproperty(obj, field))), ")
  end
  # last field extra
  field = field_names[end]
  print(io, "$field=$(repr(getproperty(obj, field)))")
  print(io, ")")
end

# Multiline version, following https://docs.julialang.org/en/v1/manual/types/#man-custom-pretty-printing-1
function Base.show(io::IO, ::MIME"text/plain", obj::Union{ExprParserWithParsed, ExprParsed})
  type_name = typeof(obj).name.name
  println(io, "EP.$type_name(")
  field_lengths = length.(string.(fieldnames(typeof(obj))))
  max_field_length = maximum(field_lengths)
  for (field, field_length) in zip(fieldnames(typeof(obj)), field_lengths)
    filler = join(fill(" ", max_field_length - field_length))
    println(io, "  $field $filler= $(repr(getproperty(obj, field)))")
  end
  println(io, ")")
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
parse_expr(parser::Expr, expr::Base.Expr) = parse_expr(parser, head = expr.head, args = expr.args)
function to_expr(parsed::Expr_Parsed)
  if parsed.args isa Block_Parsed
    Base.Expr(to_expr(parsed.head), to_expr(parsed.args.exprs)...)
  else
    Base.Expr(to_expr(parsed.head), to_expr(parsed.args)...)
  end
end


# Block
# -----

"""
parses standard blocks of code or Vectors of Base.Expr
"""
@def_structequal struct Block <: ExprParserWithParsed
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
to_expr(parsed::Block_Parsed) = Base.Expr(:block, map(to_expr, parsed.exprs)...)

function parse_expr(parser::Block, expr::Base.Expr)
  @passert expr.head == :block
  parse_expr(parser, expr.args)
end
function parse_expr(parser::Block, exprs::Union{Vector, Tuple})
  parsed_expr = if parser.ignore_linenumbernodes
    parse_expr_ignore_LineNumberNodes(parser.exprs, exprs)
  else
    parse_expr(parser.exprs, exprs)
  end
  Block_Parsed(parsed_expr)
end

parse_expr_ignore_LineNumberNodes(parser, expr) = parse_expr(parser, expr)
function parse_expr_ignore_LineNumberNodes(parsers::Union{Vector, Tuple, Iterator}, exprs::Tuple)
  parse_expr_ignore_LineNumberNodes(parsers, collect(exprs))
end
function parse_expr_ignore_LineNumberNodes(parsers::Union{Vector, Tuple, Iterator}, exprs::Vector)
  exprs_parsed = similar(exprs)
  parsers_filtered = Iterators.filter(x -> !isa(x, LineNumberNode), parsers)
  next_parser = iterate(parsers_filtered)
  for (i, expr) in enumerate(exprs)
    if expr isa LineNumberNode
      exprs_parsed[i] = expr
    else
      @passert issomething(next_parser) "Found expr `$expr` in exprs $exprs without a match in the parsers $parsers."
      parser, state = next_parser
      exprs_parsed[i] = parse_expr_ignore_LineNumberNodes(parser, expr)
      next_parser = iterate(parsers_filtered, state)
    end
  end
  @passert isnothing(next_parser) "While exprs $exprs is exhausted, there is still a left over parser $(next_parser[1]) in the parsers $parsers."
  exprs_parsed
end
function parse_expr_ignore_LineNumberNodes(parser::Expr, expr::Expr)
  @passert parser.head == expr.head
  Base.Expr(parser.head, parse_expr_ignore_LineNumberNodes(parser.args, expr.args)...)
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
to_expr(parsed::Macro_Parsed) = Base.Expr(:macrocall, Base.Symbol("@", parsed.name), parsed.linenumber, parsed.args...)
function parse_expr(parser::Macro, expr::Base.Expr)
  @passert expr.head == :macrocall
  name_with_at = expr.args[1]
  name_without_at = Base.Symbol(string(name_with_at)[2:end])
  parse_expr(parser, name = name_without_at, args = expr.args[3:end], linenumber = expr.args[2])
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
to_expr(parsed::Assignment_Parsed) = :($(parsed.left) = $(parsed.right))
function parse_expr(parser::Assignment, expr::Base.Expr)
  @passert expr.head == :(=)  "assignment head not found"
  parse_expr(parser, left = expr.args[1], right = expr.args[2])
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
function parse_expr(parser::NestedDot, expr::Base.Expr)
  @passert expr.head == :.
  exprs = _collect_nested_dots(expr)
  parse_expr(parser, base = exprs[1], properties = exprs[2:end])
end
_collect_nested_dots(expr::Base.Expr) = _collect_nested_dots(expr, Val{expr.head}())
function _collect_nested_dots(expr::Base.Expr, ::Val{:.})
  @passert length(expr.args) == 2
  @passert expr.args[2] isa QuoteNode
  [_collect_nested_dots(expr.args[1]); expr.args[2].value]
end
_collect_nested_dots(expr::Base.Expr, _::Val) = [expr]
_collect_nested_dots(any) = [any]

to_expr(parsed::NestedDot_Parsed) = foldl(parsed.properties; init=parsed.base) do expr, property
  Base.Expr(:., expr, QuoteNode(property))
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

parse_expr(parser::Reference, expr::Base.Symbol) = parse_expr(parser, name = expr, curlies = [])
function parse_expr(parser::Reference, expr::Base.Expr)
  if expr.head == :curly
    parse_expr(parser, name = expr.args[1], curlies = expr.args[2:end])
  elseif expr.head == :.
    parse_expr(parser, name = expr, curlies = [])
  else
    throw(ParseError("Cannot parse expression as reference: $expr"))
  end
end

function to_expr(parsed::Reference_Parsed)
  if isempty(parsed.curlies)
    to_expr(parsed.name)
  else
    :( $(to_expr(parsed.name)){$(to_expr(parsed.curlies)...)} )
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

function parse_expr(parser::Call, expr::Base.Expr)
  @passert expr.head == :call
  called = parse_expr(Reference(), expr.args[1])
  args, kwargs = if _is_kws(expr.args[2])
    expr.args[3:end], expr.args[2].args
  else
    expr.args[2:end], []
  end
  parse_expr(parser, name = called.name, curlies = called.curlies, args = args, kwargs = kwargs)
end

to_expr(parsed::Call_Parsed) = _to_expr_Call(parsed, Val{nonempty(parsed.curlies)}(), Val{nonempty(parsed.kwargs)}())
_to_expr_Call(parsed, iscurlies::True, iswheres::True) = :(
  $(to_expr(parsed.name)){$(to_expr(parsed.curlies)...)}($(to_expr(parsed.args)...),;$(to_expr(parsed.kwargs)...))
)
_to_expr_Call(parsed, iscurlies::True, iswheres::False) = :(
  $(to_expr(parsed.name)){$(to_expr(parsed.curlies)...)}($(to_expr(parsed.args)...),)
)
_to_expr_Call(parsed, iscurlies::False, iswheres::True) = :(
  $(to_expr(parsed.name))($(to_expr(parsed.args)...),;$(to_expr(parsed.kwargs)...))
)
_to_expr_Call(parsed, iscurlies::False, iswheres::False) = :(
  $(to_expr(parsed.name))($(to_expr(parsed.args)...),)
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

function parse_expr(parser::Signature, expr::Base.Expr)
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
  parse_expr(parser, name = name, curlies = curlies, args = args, kwargs = kwargs, wheres = wheres)
end

to_expr(parsed::Signature_Parsed) = _to_expr_Signature(parsed, parsed.name, Val{nonempty(parsed.curlies)}(), Val{nonempty(parsed.kwargs)}(), Val{nonempty(parsed.wheres)}())
_to_expr_Signature(parsed, ::Nothing, iscurlies::True, _, _) = error("Impossible Reached: Given curlies but no name. parsed.curlies = $(parsed.curlies)")
_to_expr_Signature(parsed, ::Nothing, ::False, iskwargs::False, iswheres::False) = :(
  ($(to_expr(parsed.args)...),)
)
_to_expr_Signature(parsed, ::Nothing, ::False, iskwargs::True, iswheres::False) = :(
  ($(to_expr(parsed.args)...), ; $(to_expr(parsed.kwargs)...))
)
_to_expr_Signature(parsed, ::Nothing, ::False, iskwargs::False, iswheres::True) = :(
  ($(to_expr(parsed.args)...),) where {$(to_expr(parsed.wheres)...)}
)
_to_expr_Signature(parsed, ::Nothing, ::False, iskwargs::True, iswheres::True) = :(
  ($(to_expr(parsed.args)...),; $(to_expr(parsed.kwargs)...)) where {$(to_expr(parsed.wheres)...)}
)
_to_expr_Signature(parsed, name, iscurlies::True, iskwargs::True, iswheres::True) = :(
  $(to_expr(parsed.name)){$(to_expr(parsed.curlies)...)}($(to_expr(parsed.args)...), ; $(to_expr(parsed.kwargs)...)) where {$(to_expr(parsed.wheres)...)}
)
_to_expr_Signature(parsed, name, iscurlies::True, iskwargs::True, iswheres::False) = :(
  $(to_expr(parsed.name)){$(to_expr(parsed.curlies)...)}($(to_expr(parsed.args)...), ; $(to_expr(parsed.kwargs)...))
)
_to_expr_Signature(parsed, name, iscurlies::True, iskwargs::False, iswheres::True) = :(
  $(to_expr(parsed.name)){$(to_expr(parsed.curlies)...)}($(to_expr(parsed.args)...),) where {$(to_expr(parsed.wheres)...)}
)
_to_expr_Signature(parsed, name, iscurlies::True, iskwargs::False, iswheres::False) = :(
  $(to_expr(parsed.name)){$(to_expr(parsed.curlies)...)}($(to_expr(parsed.args)...),)
)
_to_expr_Signature(parsed, name, iscurlies::False, iskwargs::True, iswheres::True) = :(
  $(to_expr(parsed.name))($(to_expr(parsed.args)...), ; $(to_expr(parsed.kwargs)...)) where {$(to_expr(parsed.wheres)...)}
)
_to_expr_Signature(parsed, name, iscurlies::False, iskwargs::True, iswheres::False) = :(
  $(to_expr(parsed.name))($(to_expr(parsed.args)...), ; $(to_expr(parsed.kwargs)...))
)
_to_expr_Signature(parsed, name, iscurlies::False, iskwargs::False, iswheres::True) = :(
  $(to_expr(parsed.name))($(to_expr(parsed.args)...),) where {$(to_expr(parsed.wheres)...)}
)
_to_expr_Signature(parsed, name, iscurlies::False, iskwargs::False, iswheres::False) = :(
  $(to_expr(parsed.name))($(to_expr(parsed.args)...),)
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

function parse_expr(parser::Function, expr::Base.Expr)
  @passert expr.head in (:function, :(=))
  @passert length(expr.args) == 2
  body = expr.args[2]

  signature = parse_expr(Signature(), expr.args[1])
  parse_expr(parser,
    name = signature.name,
    curlies = signature.curlies,
    args = signature.args,
    kwargs = signature.kwargs,
    wheres = signature.wheres,
    body = body)
end

to_expr(parsed::Function_Parsed) = Base.Expr(:function, to_expr(Signature_Parsed(parsed)), to_expr(parsed.body))

Signature_Parsed(parsed::Function_Parsed) = Signature_Parsed(
  name = parsed.name,
  curlies = parsed.curlies,
  args = parsed.args,
  kwargs = parsed.kwargs,
  wheres = parsed.wheres,
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

parse_expr(parser::Type, expr::Base.Symbol) = parse_expr(parser, name = expr, curlies = [], wheres = [])
parse_expr(parser::Type, expr::Base.Type) = parse_expr(parser, name = expr, curlies = [], wheres = [])
function parse_expr(parser::Type, expr::Base.Expr)
  expr′, wheres = split_where(expr)
  ref = parse_expr(Reference(), expr′)
  parse_expr(parser, name = ref.name, curlies = ref.curlies, wheres = wheres)
end

to_expr(parsed::Type_Parsed) = _to_expr_Type(parsed, Val{nonempty(parsed.curlies)}(), Val{nonempty(parsed.wheres)}())
_to_expr_Type(parsed, curlies::True, wheres::True) = :(
  $(to_expr(parsed.name)){$(to_expr(parsed.curlies)...)} where {$(to_expr(parsed.wheres)...)}
)
_to_expr_Type(parsed, curlies::True, wheres::False) = :(
  $(to_expr(parsed.name)){$(to_expr(parsed.curlies)...)}
)
_to_expr_Type(parsed, curlies::False, wheres::True) = :(
  $(to_expr(parsed.name)) where {$(to_expr(parsed.wheres))}
)
_to_expr_Type(parsed, curlies::False, wheres::False) = to_expr(parsed.name)


# TypeRange
# ---------

"""
parses:
```
TypeVar >: LowerBound
TypeVar <: UpperBound
LowerBound <: TypeVar <: UpperBound
```

Note: Construct with `typevar = anysymbol` to guarantee that only plain symbols can be used as type variable
"""
@exprparser struct TypeRange
  # naming is analog to Base.TypeVar
  lb = anything
  name = anything
  ub = anything
end

function parse_expr(parser::TypeRange, expr::Base.Expr)
  if expr.head == :>:
    parse_expr(parser, lb = expr.args[2], name = expr.args[1], ub = Any)
  elseif expr.head == :<:
    parse_expr(parser, lb = Union{}, name = expr.args[1], ub = expr.args[2])
  elseif expr.head == :comparison && length(expr.args) == 5 && expr.args[2] == expr.args[4] == :<:
    parse_expr(parser, lb = expr.args[1], name = expr.args[3], ub = expr.args[5])
  else
    throw(ParseError("TypeRange only parses <:, >: constructs, but got: $expr"))
  end
end

to_expr(parsed::TypeRange_Parsed) = _to_expr_TypeRange(parsed.lb, parsed.name, parsed.ub)
_to_expr_TypeRange(::Base.Type{Union{}}, name, ::Base.Type{Any}) = name
_to_expr_TypeRange(lb, name, ::Base.Type{Any}) = :($name >: $lb)
_to_expr_TypeRange(::Base.Type{Union{}}, name, ub) = :($name <: $ub)
_to_expr_TypeRange(lb, name, ub) = :($lb <: $name <: $ub)


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

function parse_expr(parser::TypeAnnotation, expr::Base.Expr)
  @passert expr.head == :(::)
  @passert length(expr.args) <= 2
  if length(expr.args) == 1
    parse_expr(parser, name = nothing, type = expr.args[1])
  else
    parse_expr(parser, name = expr.args[1], type = expr.args[2])
  end
end
function to_expr(parsed::TypeAnnotation_Parsed)
  if isnothing(parsed.name)
    :(::$(to_expr(parsed.type)))
  else
    :($(to_expr(parsed.name))::$(to_expr(parsed.type)))
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

function parse_expr(parser::Arg, expr::Base.Symbol)
  # don't use the parser for `default`
  Arg_Parsed(
    name = parse_expr(parser.name, expr),
    type = parse_expr(parser.type, Any),
    default = nodefault)
end

const _arg_left_handside_parser = AnyOf(anysymbol, TypeAnnotation())
function parse_expr(parser::Arg, expr::Base.Expr)
  if expr.head == :kw
    name, type = @match(parse_expr(_arg_left_handside_parser, expr.args[1])) do f
      f(symbol::Base.Symbol) = symbol, Any
      function f(parsed::TypeAnnotation_Parsed)
        @passert issomething(parsed.name) "need variable name if default is given"
        parsed.name, parsed.type
      end
    end
    parse_expr(parser, name = name, type = type, default = expr.args[2])
  elseif expr.head == :...
    # Vararg is not a valid Type https://github.com/JuliaLang/julia/issues/30995
    parse_expr(parser, name = expr.args[1], type = Vararg, default = nodefault)
  else
    name, type = @match(parse_expr(_arg_left_handside_parser, expr)) do f
      f(symbol::Base.Symbol) = symbol, Any
      f(parsed::TypeAnnotation_Parsed) = parsed.name, parsed.type
    end
    # don't use the parser for `default`
    Arg_Parsed(
      name = parse_expr(parser.name, name),
      type = parse_expr(parser.type, type),
      default = nodefault)
  end
end

to_expr(parsed::Arg_Parsed) = _to_expr_Arg(parsed.name, parsed.type, parsed.default)
_to_expr_Arg(::Nothing, type, ::NoDefault) = :(::$(to_expr(type)))
_to_expr_Arg(name, ::Base.Type{Any}, ::NoDefault) = to_expr(name)
function _to_expr_Arg(name, type, ::NoDefault)
  if type === Vararg
    Base.Expr(:..., to_expr(name))
  else
    :($(to_expr(name))::$(to_expr(type)))
  end
end
_to_expr_Arg(name, ::Base.Type{Any}, default) = Base.Expr(:kw, to_expr(name), to_expr(default))
_to_expr_Arg(name, type, default) = Base.Expr(:kw, :($(to_expr(name))::$(to_expr(type))), to_expr(default))
