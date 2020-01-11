module Parsers
export toAST, @parserfactory, Parser, Parsed,
  Named, Named_Parsed, Indexed, Indexed_Parsed,
  nodefault, NoDefault

using ASTParser
using Base.Iterators
using SimpleMatch
using StructEquality
using ProxyInterface
const match = ASTParser.match

include("Utils.jl")
using .Utils
include("syntax.jl")

# Main Interface
# ==============

"""
  convert called information back to AbstractSyntaxTree

defaults to returning same value
however if something knows about how it can be translated, just overload the function
"""
toAST(a) = a
toAST(a::Union{Vector, Tuple}) = map(toAST, a)
toAST(a::Base.Expr) = Base.Expr(a.head, toAST(a.args)...)




"""
  This is mainly for internal use. Please see @parserfactory for the public interface

Types which constructor creates a Parser that can parses Base.Expr
into a struct with identical fields, where then the parsed values will be stored.
``Parsed(MyParser)`` will return the Parsedtype
"""
abstract type Parser <: Matchers.Matcher end

"""
  maps Parser to respective Parsed type

and is also abstract super type of all Parsed types

Example
```
Parsed(Parsers.Symbol) == Parsers.Symbol_Parsed
```
"""
abstract type Parsed end

Matchers.match(parser::Parser, value) = parser(value)


# Generic Parser Helper
# =====================

"""
adds a Mapping Layer to a possibly nested Parser with which you can refer into a deep nested subparser by name
"""
@def_structequal struct Indexed{T} <: Parser
  _mapping::Dict
  _paths::Dict
  expr::T
  function Indexed(func_dict!)
    mapping = Dict()
    expr = func_dict!(mapping)
    paths = Dict(k => find_object(v, expr) for (k,v) in mapping)
    new{typeof(expr)}(mapping, paths, expr)
  end
end

@def_structequal struct Indexed_Parsed{T} <: Parsed
  _mapping::Dict
  _paths::Dict
  expr
end
Parsed(::Base.Type{Indexed}) = Indexed_Parsed
Parsed(::Base.Type{Indexed{T}}) where T = Indexed_Parsed{T}
toAST(ipp::Parsed(Indexed)) = toAST(ipp.expr)

function (ip::Indexed{T})(expr) where T
  parsed = match(ip.expr, expr)
  mapping = Dict(k => get_object(nested_index, parsed) for (k, nested_index) in ip._paths)
  Indexed_Parsed{T}(mapping, ip._paths, parsed)
end

ProxyInterface.dict(parser::Indexed) = parser._mapping
ProxyInterface.dict(::Base.Type{Indexed{T}}) where T = T
ProxyInterface.@dict Indexed

ProxyInterface.dict(parsed::Indexed_Parsed) = parsed._mapping
ProxyInterface.dict(::Base.Type{Indexed_Parsed{T}}) where T = T
ProxyInterface.@dict Indexed_Parsed


"""
  identify Parser by Tag

Named Parsers can be easily identified by Type. The Nametag is also passed to the parsed value.
"""
@def_structequal struct Named{Name, T} <: Parser
  expr::T
  Named{Name}(parser::T) where {Name, T} = new{Name, T}(parser)
end
@def_structequal struct Named_Parsed{Name, T} <: Parsed
  expr
end
Parsed(::Base.Type{Named}) = Named_Parsed
Parsed(::Base.Type{Named{Name}}) where Name = Named_Parsed{Name}
Parsed(::Base.Type{Named{Name, T}}) where {Name, T} = Named_Parsed{Name, T}

toAST(npp::Parsed(Named)) = toAST(npp.expr)
function (np::Named{Name, T})(expr) where {Name, T}
  parsed = match(np.expr, expr)
  Parsed(Named){Name, T}(parsed)
end


# Generic Parsers
# ===============

"""
parses:
```
Base.Expr(head, args...)
```
"""
@parserfactory struct Expr
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
(p::Expr)(expr::Base.Expr) = p(head = expr.head, args = expr.args)
function toAST(pp::Parsed(Expr))
  if pp.args isa Parsed(Block)
    Base.Expr(toAST(pp.head), toAST(pp.args.exprs)...)
  else
    Base.Expr(toAST(pp.head), toAST(pp.args)...)
  end
end





# Specific Parser
# ===============

"""
parses only symbols
"""
@parserfactory struct Symbol
  symbol = anything
end

(p::Symbol)(symbol::Base.Symbol) = p(symbol = symbol)
(p::Symbol)(_) = throw(ParseError("Symbol only parses Symbols"))
toAST(pp::Parsed(Symbol)) = toAST(pp.symbol)




"""
parses standard blocks of code or Vectors of Base.Expr
"""
@def_structequal struct Block <: Parser
  exprs
  ignore_linenumbernodes::Bool # TODO be able to deal with LineNumberNode in a transparent way such that they are restored after parsing

  Block(exprs...; ignore_linenumbernodes=true) = new(exprs, ignore_linenumbernodes)
  Block() = new(Matchers.Iterator(repeated(anything)), true)
end

@def_structequal mutable struct Block_Parsed <: Parsed
  exprs
end
Parsed(::Base.Type{Block}) = Block_Parsed
toAST(bpp::Parsed(Block)) = Base.Expr(:block, map(toAST, bpp.exprs)...)

match_ignore_LineNumberNodes(a, b) = match(a, b)
match_ignore_LineNumberNodes(args1::Union{Vector, Tuple}, args2::Tuple) = match_ignore_LineNumberNodes(args1, collect(args2))
function match_ignore_LineNumberNodes(args1::Union{Vector, Tuple}, args2::Vector)
  args_match = similar(args2)
  args1_filtered = Iterators.filter(x -> !isa(x, LineNumberNode), args1)
  next1 = iterate(args1_filtered)
  for (i, a2) in enumerate(args2)
    if a2 isa LineNumberNode
      args_match[i] = a2
    else
      @passert issomething(next1) "found element `$a2` in args2 $args2 without a match in args1 $args1."
      a1, a1_state = next1
      args_match[i] = match_ignore_LineNumberNodes(a1, a2)
      next1 = iterate(args1_filtered, a1_state)
    end
  end
  @passert isnothing(next1) "While args2 $args2 is exhausted, there is still a rest $next1 in args1 $args1"
  args_match
end
function match_ignore_LineNumberNodes(expr1::Expr, expr2::Expr)
  @passert expr1.head == expr2.head
  Expr(expr1.head, match_ignore_LineNumberNodes(expr1.args, expr2.args)...)
end

function (bp::Block)(exprs::Union{Vector, Tuple})
  parsed_expr = if bp.ignore_linenumbernodes
    match_ignore_LineNumberNodes(bp.exprs, exprs)
  else
    match(bp.exprs, exprs)
  end
  Block_Parsed(parsed_expr)
end
function (bp::Block)(expr::Base.Expr)
  @passert expr.head == :block
  bp(expr.args)
end




"""
parses:
```
@macroname arg1 arg2 ...
```
"""
@parserfactory struct Macro
  name = anything
  args = anything
  linenumber = Matchers.Type(LineNumberNode)
end
toAST(mpp::Parsed(Macro)) = Base.Expr(:macrocall, Base.Symbol("@", mpp.name), mpp.linenumber, mpp.args...)
function (mp::Macro)(expr::Base.Expr)
  @passert expr.head == :macrocall
  name_with_at = expr.args[1]
  name_without_at = Base.Symbol(string(name_with_at)[2:end])
  mp(name = name_without_at, args = expr.args[3:end], linenumber = expr.args[2])
end


"""
parses:
```
left = right
```
"""
@parserfactory struct Assignment
  left = anything
  right = anything
end
toAST(app::Parsed(Assignment)) = :($(app.left) = $(app.right))
function (ap::Assignment)(expr::Base.Expr)
  @passert expr.head == :(=)  "assignment head not found"
  ap(left = expr.args[1], right = expr.args[2])
end

"""
parses:
```
a.b
fun(T{:hi}).b.c.d.e.f
```
"""
@parserfactory struct NestedDot
  base = anything
  properties = anything
end
function (ndp::NestedDot)(expr::Base.Expr)
  @passert expr.head == :.
  exprs = _collect_nested_dots(expr)
  ndp(base = exprs[1], properties = exprs[2:end])
end
_collect_nested_dots(expr::Base.Expr) = _collect_nested_dots(expr, Val{expr.head}())
function _collect_nested_dots(expr::Base.Expr, ::Val{:.})
  @passert length(expr.args) == 2
  @passert expr.args[2] isa QuoteNode
  [_collect_nested_dots(expr.args[1]); expr.args[2].value]
end
_collect_nested_dots(expr::Base.Expr, _::Val) = [expr]
_collect_nested_dots(any) = [any]
toAST(ndpp::Parsed(NestedDot)) = foldl(ndpp.properties; init=ndpp.base) do a, b
  Base.Expr(:., a, QuoteNode(b))
end



"""
parses:
```
a
a{b, c}
```
"""
@parserfactory struct Reference
  name = anything
  curlies = anything
end

(p::Reference)(e::Base.Symbol) = p(name = e, curlies = [])

function (p::Reference)(e::Base.Expr)
  if e.head == :curly
    p(name = e.args[1], curlies = e.args[2:end])
  elseif e.head == :.
    p(name = e, curlies = [])
  else
    throw(ParseError("Cannot parse expression as reference: $e"))
  end
end

function toAST(pp::Parsed(Reference))
  if isempty(pp.curlies)
    toAST(pp.name)
  else
    :( $(toAST(pp.name)){$(toAST(pp.curlies)...)} )
  end
end



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
@parserfactory struct Call
  name = anything
  curlies = anything
  args = anything
  kwargs = anything
end

function (p::Call)(e::Base.Expr)
  @passert e.head == :call
  called = Reference()(e.args[1])
  args, kwargs = if _is_kws(e.args[2])
    e.args[3:end], e.args[2].args
  else
    e.args[2:end], []
  end
  p(name = called.name, curlies = called.curlies, args = args, kwargs = kwargs)
end

toAST(pp::Parsed(Call)) = toAST_Call(pp, Val{nonempty(pp.curlies)}(), Val{nonempty(pp.kwargs)}())
toAST_Call(pp, iscurlies::True, iswheres::True) = :(
  $(toAST(pp.name)){$(toAST(pp.curlies)...)}($(toAST(pp.args)...),;$(toAST(pp.kwargs)...))
)
toAST_Call(pp, iscurlies::True, iswheres::False) = :(
  $(toAST(pp.name)){$(toAST(pp.curlies)...)}($(toAST(pp.args)...),)
)
toAST_Call(pp, iscurlies::False, iswheres::True) = :(
  $(toAST(pp.name))($(toAST(pp.args)...),;$(toAST(pp.kwargs)...))
)
toAST_Call(pp, iscurlies::False, iswheres::False) = :(
  $(toAST(pp.name))($(toAST(pp.args)...),)
)


"""
parses:
```
function a(b, c) where B
  d
end
a(b, c) = d
```
"""
@parserfactory struct Function
  name = anything
  curlies = anything
  args = anything
  kwargs = anything
  wheres = anything
  body = anything
end

function (p::Function)(expr::Base.Expr)
  @passert expr.head in (:function, :(=))
  @passert length(expr.args) == 2
  body = expr.args[2]

  # we don't reuse ParsedCallExpr because functions can actually be anonymous
  call, wheres = split_where(expr.args[1])
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
    parsed = Call()(call)
    parsed.name, parsed.curlies, parsed.args, parsed.kwargs
  end
  p(name = name, curlies = curlies, args = args, kwargs = kwargs, wheres = wheres, body = body)
end


toAST(pp::Parsed(Function)) = _toAST_Function(pp, pp.name, Val{nonempty(pp.curlies)}(), Val{nonempty(pp.kwargs)}(), Val{nonempty(pp.wheres)}())
_toAST_Function(pp, ::Nothing, iscurlies::True, _, _) = error("Impossible Reached: Given curlies but no name. pp.curlies = $(pp.curlies)")
_toAST_Function(pp, ::Nothing, ::False, iskwargs::False, iswheres::False) = Base.Expr(
  :function,
  :(($(toAST(pp.args)...),)),
  toAST(pp.body)
)
_toAST_Function(pp, ::Nothing, ::False, iskwargs::True, iswheres::False) = Base.Expr(
  :function,
  :(($(toAST(pp.args)...), ; $(toAST(pp.kwargs)...))),
  toAST(pp.body)
)
_toAST_Function(pp, ::Nothing, ::False, iskwargs::False, iswheres::True) = Base.Expr(
  :function,
  :(($(toAST(pp.args)...),) where {$(toAST(pp.wheres)...)}),
  toAST(pp.body)
)
_toAST_Function(pp, ::Nothing, ::False, iskwargs::True, iswheres::True) = Base.Expr(
  :function,
  :(($(toAST(pp.args)...),; $(toAST(pp.kwargs)...)) where {$(toAST(pp.wheres)...)}),
  toAST(pp.body)
)
_toAST_Function(pp, name, iscurlies::True, iskwargs::True, iswheres::True) = Base.Expr(
  :function,
  :($(toAST(pp.name)){$(toAST(pp.curlies)...)}($(toAST(pp.args)...), ; $(toAST(pp.kwargs)...)) where {$(toAST(pp.wheres)...)}),
  toAST(pp.body)
)
_toAST_Function(pp, name, iscurlies::True, iskwargs::True, iswheres::False) = Base.Expr(
  :function,
  :($(toAST(pp.name)){$(toAST(pp.curlies)...)}($(toAST(pp.args)...), ; $(toAST(pp.kwargs)...))),
  toAST(pp.body)
)
_toAST_Function(pp, name, iscurlies::True, iskwargs::False, iswheres::True) = Base.Expr(
  :function,
  :($(toAST(pp.name)){$(toAST(pp.curlies)...)}($(toAST(pp.args)...),) where {$(toAST(pp.wheres)...)}),
  toAST(pp.body)
)
_toAST_Function(pp, name, iscurlies::True, iskwargs::False, iswheres::False) = Base.Expr(
  :function,
  :($(toAST(pp.name)){$(toAST(pp.curlies)...)}($(toAST(pp.args)...),)),
  toAST(pp.body)
)
_toAST_Function(pp, name, iscurlies::False, iskwargs::True, iswheres::True) = Base.Expr(
  :function,
  :($(toAST(pp.name))($(toAST(pp.args)...), ; $(toAST(pp.kwargs)...)) where {$(toAST(pp.wheres)...)}),
  toAST(pp.body)
)
_toAST_Function(pp, name, iscurlies::False, iskwargs::True, iswheres::False) = Base.Expr(
  :function,
  :($(toAST(pp.name))($(toAST(pp.args)...), ; $(toAST(pp.kwargs)...))),
  toAST(pp.body)
)
_toAST_Function(pp, name, iscurlies::False, iskwargs::False, iswheres::True) = Base.Expr(
  :function,
  :($(toAST(pp.name))($(toAST(pp.args)...),) where {$(toAST(pp.wheres)...)}),
  toAST(pp.body)
)
_toAST_Function(pp, name, iscurlies::False, iskwargs::False, iswheres::False) = Base.Expr(
  :function,
  :($(toAST(pp.name))($(toAST(pp.args)...),)),
  toAST(pp.body)
)



"""
parses:
```
a
a{b, c}
a{d} where d
```
"""
@parserfactory struct Type
  name = anything
  curlies = anything
  wheres = anything
end

(p::Type)(e::Base.Symbol) = p(name = e, curlies = [], wheres = [])
(p::Type)(e::Base.Type) = p(name = e, curlies = [], wheres = [])
function (p::Type)(e::Base.Expr)
  e′, wheres = split_where(e)
  ref = Reference()(e′)
  p(name = ref.name, curlies = ref.curlies, wheres = wheres)
end

toAST(pp::Parsed(Type)) = toAST_Type(pp, Val{nonempty(pp.curlies)}(), Val{nonempty(pp.wheres)}())
toAST_Type(pp, curlies::True, wheres::True) = :(
  $(toAST(pp.name)){$(toAST(pp.curlies)...)} where {$(toAST(pp.wheres)...)}
)
toAST_Type(pp, curlies::True, wheres::False) = :(
  $(toAST(pp.name)){$(toAST(pp.curlies)...)}
)
toAST_Type(pp, curlies::False, wheres::True) = :(
  $(toAST(pp.name)) where {$(toAST(pp.wheres))}
)
toAST_Type(pp, curlies::False, wheres::False) = toAST(pp.name)


"""
parses:
```
TypeVar >: LowerBound
TypeVar <: UpperBound
LowerBound <: TypeVar <: UpperBound
```

Note: Construct with typevar = Parsers.Symbol() to guarantee that only plain symbols can be used as type variable
"""
@parserfactory struct TypeRange
  # naming is analog to Base.TypeVar
  lb = anything
  name = anything
  ub = anything
end

toAST(trpp::Parsed(TypeRange)) = toAST_TypeRange(trpp.lb, trpp.name, trpp.ub)
toAST_TypeRange(::Base.Type{Union{}}, name, ::Base.Type{Any}) = name
toAST_TypeRange(lb, name, ::Base.Type{Any}) = :($name >: $lb)
toAST_TypeRange(::Base.Type{Union{}}, name, ub) = :($name <: $ub)
toAST_TypeRange(lb, name, ub) = :($lb <: $name <: $ub)

function (trp::TypeRange)(expr::Base.Expr)
  if expr.head == :>:
    trp(lb = expr.args[2], name = expr.args[1], ub = Any)
  elseif expr.head == :<:
    trp(lb = Union{}, name = expr.args[1], ub = expr.args[2])
  elseif expr.head == :comparison && length(expr.args) == 5 && expr.args[2] == expr.args[4] == :<:
    trp(lb = expr.args[1], name = expr.args[3], ub = expr.args[5])
  else
    throw(ParseError("TypeRange only parses <:, >: constructs, but got: $expr"))
  end
end


"""
parses:
```
a
a::B
::B
```
"""
@parserfactory struct TypeAnnotation
  # name == nothing will be treated as anonymous type annotation
  name = anything
  type = anything
end

function (p::TypeAnnotation)(e::Base.Expr)
  @passert e.head == :(::)
  @passert length(e.args) <= 2
  if length(e.args) == 1
    p(name = nothing, type = e.args[1])
  else
    p(name = e.args[1], type = e.args[2])
  end
end
function toAST(pp::Parsed(TypeAnnotation))
  if isnothing(pp.name)
    :(::$(toAST(pp.type)))
  else
    :($(toAST(pp.name))::$(toAST(pp.type)))
  end
end

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
@parserfactory struct Arg
  # name == nothing will be treated as anonymous type annotation
  name = anything
  type = anything
  default = anything
end

function (p::Arg)(e::Base.Symbol)
  # don't use the parser for ``default``
  Parsed(Arg)(
    name = match(p.name, e),
    type = match(p.type, Any),
    default = nodefault)
end

const _arg_left_handside_parser = Matchers.AnyOf(Symbol(), TypeAnnotation())
function (p::Arg)(expr::Base.Expr)
  if expr.head == :kw
    name, type = @match(_arg_left_handside_parser(expr.args[1])) do f
      f(s::Symbol_Parsed) = s.symbol, Any
      function f(a::TypeAnnotation_Parsed)
        @passert issomething(a.name) "need variable name if default is given"
        a.name, a.type
      end
    end
    p(name = name, type = type, default = expr.args[2])
  elseif expr.head == :...
    # Vararg is not a valid Type https://github.com/JuliaLang/julia/issues/30995
    p(name = expr.args[1], type = Vararg, default = nodefault)
  else
    name, type = @match(_arg_left_handside_parser(expr)) do f
      f(s::Symbol_Parsed) = s.symbol, Any
      f(a::TypeAnnotation_Parsed) = a.name, a.type
    end
    # don't use the parser for ``default``
    Arg_Parsed(
      name = match(p.name, name),
      type = match(p.type, type),
      default = nodefault)
  end
end

toAST(pp::Parsed(Arg)) = _toAST_Arg(pp.name, pp.type, pp.default)
_toAST_Arg(::Nothing, type, ::NoDefault) = :(::$(toAST(type)))
_toAST_Arg(name, ::Base.Type{Any}, ::NoDefault) = toAST(name)
function _toAST_Arg(name, type, ::NoDefault)
  if type === Vararg
    Base.Expr(:..., toAST(name))
  else
    :($(toAST(name))::$(toAST(type)))
  end
end
_toAST_Arg(name, ::Base.Type{Any}, default) = Base.Expr(:kw, toAST(name), toAST(default))
_toAST_Arg(name, type, default) = Base.Expr(:kw, :($(toAST(name))::$(toAST(type))), toAST(default))

end  # module
