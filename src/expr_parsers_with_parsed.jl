using Base.Iterators
using SimpleMatch
using StructEquality
using ProxyInterfaces


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
  throw(ParseError("$(typeof(parser)) has no `parse_expr` method defined to capture Type `$(typeof(any))`. Got: `$any`."))
end

# Overwriting show to print content by fields instead of positions.

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
    EP.Expr(head = EP.anything, args = EP.anything)
    EP.Expr(expr; [ignore_linenumbernodes=true])

It is the most flexible parser, but hence also the least plug-and-play.

Parses the following
```julia
Base.Expr(head, args...)
```

# Examples

```julia
julia> using ExprParsers

julia> parser = EP.Expr(head = :vect);

julia> parse_expr(parser, :([1,2,3]))
EP.Expr_Parsed(
  head = :vect
  args = Any[1, 2, 3]
)
julia> parse_expr(parser, :(f(a) = a))
ERROR: ParseError: Using default `==` comparison, but parser `:vect` ≠ value `:(=)`.
```

Also see [`Indexed`](@ref) for an example to combine `EP.Indexed` with `EP.Expr`.
"""
@exprparser struct Expr
  head = anything
  args = anything = []
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

@doc raw"""
    EP.Block(block_expr; [ignore_linenumbernodes = true])
    EP.Block(expr1, expr2, ...; [ignore_linenumbernodes = true])
    EP.Block()

Helper to parse blocks of code (i.e. `expr.head == :block`) or a given list of expr respectively.

The main purpose is to handle linenumbernodes, otherwise it behaves similar to plain Vector of Expr.

Parses the following
```julia
quote
  any
  4
end
[:(a = 4), 42, :anyvector]
(:(a = 4), 42, :or_tuple_of_expr)
end
```

# Examples
```jldoctest
julia> using ExprParsers

julia> parser = EP.Block(quote
         $(EP.anything)
         $(EP.anysymbol)
         13
       end);

julia> parse_expr(parser, [:(a = 4), :hi, 13])
EP.Block_Parsed(
  exprs = Any[:(a = 4), :hi, 13]
)
julia> parse_expr(parser, quote
         whatever(a) = a
         asymbol
         14
       end)
ERROR: ParseError: Using default `==` comparison, but parser `13` ≠ value `14`.
```
Used within [`EP.Expr`](@ref).
"""
@def_structequal struct Block <: ExprParserWithParsed
  exprs
  ignore_linenumbernodes::Bool

  function Block(expr::Base.Expr; ignore_linenumbernodes = true)
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
    Macro(name = EP.anything, args = EP.anything, linenumber = EP.Isa(LineNumberNode))

Parses the following
```julia
@macroname arg1 arg2 ...
```

# Examples
```jldoctest
julia> using ExprParsers

julia> parser = EP.Macro(name = :mymacro);

julia> parse_expr(parser, :(@mymacro 1 two))
EP.Macro_Parsed(
  name       = :mymacro
  args       = Any[1, :two]
  linenumber = :(#= none:1 =#)
)
julia> parse_expr(parser, :(@anothermacro))
ERROR: ParseError: Using default `==` comparison, but parser `:mymacro` ≠ value `:anothermacro`.
```
"""
@exprparser struct Macro
  name = anything
  args = anything = []
  linenumber = Isa(LineNumberNode) = LineNumberNode(0)
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
    EP.Assignment(left = EP.anything, right = EP.anything)

Parses the following
```
left = right
```

# Examples

```jldoctest
julia> using ExprParsers

julia> parser = EP.Assignment(left = EP.anysymbol)
EP.Assignment(
  left  = ExprParsers.Isa{Symbol}()
  right = ExprParsers.Isa{Any}()
)
julia> parse_expr(parser, :(a = [1,2,3,4]))
EP.Assignment_Parsed(
  left  = :a
  right = :([1, 2, 3, 4])
)
julia> parse_expr(parser, :(f(a) = a))
ERROR: ParseError: Expected type `Symbol`, got `f(a)` of type `Expr`.
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
    EP.NestedDot(base = EP.anything, properties = EP.anything)

Parses the following
```julia
a.b
fun(T{:hi}).b.c.d.e.f
```

# Examples
```jldoctest
julia> using ExprParsers; using Base.Iterators

julia> parser = EP.NestedDot(
         properties = EP.Iterator(repeated(
           EP.SatisfiesPredicate("It should start with 'a'.") do x
             startswith(string(x), "a")
           end
         ))
       );

julia> parse_expr(parser, :(fun(T{:hi}).aone.atwo.athree))
EP.NestedDot_Parsed(
  base       = :(fun(T{:hi}))
  properties = [:aone, :atwo, :athree]
)
julia> parse_expr(parser, :(fun(T{:hi}).aone.btwo.athree))
ERROR: ParseError: Predicate `#1` returned false on expr `btwo`. It should start with 'a'.
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
    EP.Reference(name = EP.anything, curlies = EP.anything)

Parses the following
```julia
a
a{b, c}
```

# Examples
```jldoctest
julia> using ExprParsers

julia> parser = EP.Reference(curlies = [:A, EP.anysymbol])
EP.Reference(
  name    = ExprParsers.Isa{Any}()
  curlies = Any[:A, ExprParsers.Isa{Symbol}()]
)
julia> parse_expr(parser, :(SomeType{A, B}))
EP.Reference_Parsed(
  name    = :SomeType
  curlies = [:A, :B]
)
julia> parse_expr(parser, :(SomeType{A}))
ERROR: ParseError: length(parser) == length(values) = false
  length(parser) = 2
  parser = Any[:A, ExprParsers.Isa{Symbol}()]
  length(values) = 1
  values = Any[:A]
julia> parse_expr(parser, :(SomeType{B, C}))
ERROR: ParseError: Using default `==` comparison, but parser `:A` ≠ value `:B`.
julia> parse_expr(parser, :(SomeType{A, 1}))
ERROR: ParseError: Expected type `Symbol`, got `1` of type `Int64`.
```
"""
@exprparser struct Reference
  name = anything
  curlies = anything = []
end

parse_expr(parser::Reference, expr::Base.Symbol) = parse_expr(parser, name = expr, curlies = [])
function parse_expr(parser::Reference, expr::Base.Expr)
  if expr.head == :curly
    parse_expr(parser, name = expr.args[1], curlies = expr.args[2:end])
  elseif expr.head == :.
    parse_expr(parser, name = expr, curlies = [])
  else
    throw(ParseError("Cannot parse expr `$expr` as reference: expr.head `$(expr.head)` not in `[:curly, :.]`."))
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

@doc raw"""
    EP.Call(
      name = EP.anything,
      curlies = EP.anything,
      args = EP.anything,
      kwargs = EP.anything)

Parses the following
```julia
a()
a(b, c)
a{b, c}(d, e)
a{b, c}(d, e, f = 1; g = :two)
```

Note that all keyword arguments are collected into the `kwargs` field, also those before `;`, corresponding to standard
julia call semantics.

Note that keyword arguments are represented using the default Expr representation
`Expr(:kw, :key, "value")`.

# Examples
```jldoctest
julia> using ExprParsers

julia> parser = EP.Call(name = :myfunc)
EP.Call(
  name    = :myfunc
  curlies = ExprParsers.Isa{Any}()
  args    = ExprParsers.Isa{Any}()
  kwargs  = ExprParsers.Isa{Any}()
)
julia> parse_expr(parser, :(myfunc(a, b, c = 1; d = :hi)))
EP.Call_Parsed(
  name    = :myfunc
  curlies = Any[]
  args    = Any[:a, :b]
  kwargs  = Any[:($(Expr(:kw, :c, 1))), :($(Expr(:kw, :d, :(:hi))))]
)
julia> parse_expr(parser, :(anotherfunc(a, b, c = 1; d = :hi)))
ERROR: ParseError: Using default `==` comparison, but parser `:myfunc` ≠ value `:anotherfunc`.
```
"""
@exprparser struct Call
  name = anything
  curlies = anything = []
  args = anything = []
  kwargs = anything = []
end

function parse_expr(parser::Call, expr::Base.Expr)
  @passert expr.head == :call
  called = parse_expr(Reference(), expr.args[1])
  args, kwargs = _extract_args_kwargs__collect_all_kw_into_kwargs(expr.args[2:end])
  parse_expr(parser, name = called.name, curlies = called.curlies, args = args, kwargs = kwargs)
end

function _extract_args_kwargs__collect_all_kw_into_kwargs(expr_args)
  args = []
  kwargs = []

  otherargs, parameters = if isempty(expr_args)
    [], []
  elseif isexpr(expr_args[1], :parameters)
    expr_args[2:end], expr_args[1].args
  else
    expr_args, []
  end

  for p in otherargs
    if isexpr(p, :kw)
      push!(kwargs, p)
    else
      push!(args, p)
    end
  end
  append!(kwargs, parameters)
  args, kwargs
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

@doc raw"""
    EP.Signature(
      name = EP.anything,
      curlies = EP.anything,
      args = EP.anything,
      kwargs = EP.anything,
      wheres = EP.anything)

Similar to [`EP.Call`](@ref) but with a couple of differences
- extra `wheres`
- the `name` field might stay empty
- `args` can also contain `Expr(:kw, key, value)` values
  (corresponds to default value syntax, which is only available in signatures)

Parses the following:
```julia
a(b, c::Any)
a(b::B, c) where B
(::Any, c::C) where {C <: Number}
f(::Any, c::C, d::Int=1; e=true) where {C <: Number}
```

# Examples
```jldoctest
julia> using ExprParsers

julia> parser = EP.Signature()
EP.Signature(
  name    = ExprParsers.Isa{Any}()
  curlies = ExprParsers.Isa{Any}()
  args    = ExprParsers.Isa{Any}()
  kwargs  = ExprParsers.Isa{Any}()
  wheres  = ExprParsers.Isa{Any}()
)
julia> parse_expr(parser, :(f{T}(a::T) where T))
EP.Signature_Parsed(
  name    = :f
  curlies = Any[:T]
  args    = Any[:(a::T)]
  kwargs  = Any[]
  wheres  = Any[:T]
)
julia> parse_expr(parser, :((a, b::T) where T))
EP.Signature_Parsed(
  name    = nothing
  curlies = Any[]
  args    = Any[:a, :(b::T)]
  kwargs  = Any[]
  wheres  = Any[:T]
)
julia> parse_expr(parser, :(f(a, b::T, c::Any=3; d=true) where T))
EP.Signature_Parsed(
  name    = :f
  curlies = Any[]
  args    = Any[:a, :(b::T), :($(Expr(:kw, :(c::Any), 3)))]
  kwargs  = Any[:($(Expr(:kw, :d, true)))]
  wheres  = Any[:T]
)
julia> parse_expr(parser, :(f(a) = a))
ERROR: ParseError: expr.head in (:where, :call, :tuple) = false
  expr.head = :(=)
  expr = :(f(a) = begin
          #= none:1 =#
          a
      end)
  (:where, :call, :tuple) = (:where, :call, :tuple)
```
"""
@exprparser struct Signature
  name = anything = nothing
  curlies = anything = []
  args = anything = []
  kwargs = anything = []
  wheres = anything = []
end

function parse_expr(parser::Signature, expr::Base.Expr)
  @passert expr.head in (:where, :call, :tuple)
  # we don't reuse ParsedCallExpr because functions-signatures can actually be anonymous
  call, wheres = split_where(expr)
  @passert call isa Base.Expr  # until before here we still match assignments
  @passert call.head in (:tuple, :call)
  isanonymous = call.head == :tuple

  name, curlies, args, kwargs = if isanonymous
    _args, _kwargs = _extract_args_kwargs__no_processing(call.args)
    nothing, [], _args, _kwargs
  else
    called = parse_expr(Reference(), call.args[1])
    _args, _kwargs = _extract_args_kwargs__no_processing(call.args[2:end])
    called.name, called.curlies, _args, _kwargs
  end
  parse_expr(parser, name = name, curlies = curlies, args = args, kwargs = kwargs, wheres = wheres)
end

function _extract_args_kwargs__no_processing(expr_args)
  args, kwargs = if isempty(expr_args)
    [], []
  elseif isexpr(expr_args[1], :parameters)
    expr_args[2:end], expr_args[1].args
  else
    expr_args, []
  end
  args, kwargs
end

to_expr(parsed::Signature_Parsed) = _to_expr_Signature(parsed, parsed.name, Val{nonempty(parsed.curlies)}(), Val{nonempty(parsed.kwargs)}(), Val{nonempty(parsed.wheres)}())
_to_expr_Signature(parsed, ::Nothing, iscurlies::True, ::False, ::False) = error("Impossible Reached: Given curlies but no name. parsed.curlies = $(parsed.curlies)")
_to_expr_Signature(parsed, ::Nothing, iscurlies::True, ::False, ::True) = error("Impossible Reached: Given curlies but no name. parsed.curlies = $(parsed.curlies)")
_to_expr_Signature(parsed, ::Nothing, iscurlies::True, ::True, ::False) = error("Impossible Reached: Given curlies but no name. parsed.curlies = $(parsed.curlies)")
_to_expr_Signature(parsed, ::Nothing, iscurlies::True, ::True, ::True) = error("Impossible Reached: Given curlies but no name. parsed.curlies = $(parsed.curlies)")
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
    EP.Function(
      name = EP.anything,
      curlies = EP.anything,
      args = EP.anything,
      kwargs = EP.anything,
      wheres = EP.anything,
      body = EP.anything)

Parses full functions. For instance
```julia
function a(b, c) where B
  d
end
a(b, c) = d
```

# Examples
```jldoctest
julia> using ExprParsers

julia> parser = EP.Function(
         args = [EP.anything for i in 1:3],
       )
EP.Function(
  name    = ExprParsers.Isa{Any}()
  curlies = ExprParsers.Isa{Any}()
  args    = [ExprParsers.Isa{Any}(), ExprParsers.Isa{Any}(), ExprParsers.Isa{Any}()]
  kwargs  = ExprParsers.Isa{Any}()
  wheres  = ExprParsers.Isa{Any}()
  body    = ExprParsers.Isa{Any}()
)
julia> parse_expr(parser, :(f(a, b, c) = a + b + c))
EP.Function_Parsed(
  name    = :f
  curlies = Any[]
  args    = [:a, :b, :c]
  kwargs  = Any[]
  wheres  = Any[]
  body    = quote
    #= none:1 =#
    a + b + c
end
)
julia> parse_expr(parser, :(
         function g(a)
           a
         end
       ))
ERROR: ParseError: length(parser) == length(values) = false
  length(parser) = 3
  parser = [ExprParsers.Isa{Any}(), ExprParsers.Isa{Any}(), ExprParsers.Isa{Any}()]
  length(values) = 1
  values = Any[:a]
julia> parse_expr(parser, :a)
ERROR: ParseError: ExprParsers.Function has no `parse_expr` method defined to capture Type `Symbol`. Got: `a`.
```
"""
@exprparser struct Function
  name = anything = nothing
  curlies = anything = []
  args = anything = []
  kwargs = anything = []
  wheres = anything = []
  body = anything = nothing
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
    EP.Type(name = EP.anything, curlies = EP.anything, wheres = EP.anything)

Parses the following:
```julia
a
a{b, c}
a{d} where d
```

# Examples

```jldoctest
julia> using ExprParsers

julia> parser = EP.Type()
EP.Type(
  name    = ExprParsers.Isa{Any}()
  curlies = ExprParsers.Isa{Any}()
  wheres  = ExprParsers.Isa{Any}()
)
julia> parse_expr(parser, :(Array{T, 2} where T))
EP.Type_Parsed(
  name    = :Array
  curlies = Any[:T, 2]
  wheres  = Any[:T]
)
julia> parse_expr(parser, :(f(1,2)))
ERROR: ParseError: Cannot parse expr `f(1, 2)` as reference: expr.head `call` not in `[:curly, :.]`.
```
"""
@exprparser struct Type
  name = anything
  curlies = anything = []
  wheres = anything = []
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
    EP.TypeRange(lb = EP.anything, name = EP.anything, ub = EP.anything)

Note: Construct with `typevar = EP.anysymbol` to guarantee that only plain symbols can be used as type variable.

Parses the following.
```
TypeVar >: LowerBound
TypeVar <: UpperBound
LowerBound <: TypeVar <: UpperBound
```

# Examples
```
julia> using ExprParsers

julia> parser = EP.TypeRange(name = EP.anysymbol)
EP.TypeRange(
  lb   = ExprParsers.Isa{Any}()
  name = ExprParsers.Isa{Symbol}()
  ub   = ExprParsers.Isa{Any}()
)
julia> parse_expr(parser, :(Int <: T <: Number))
EP.TypeRange_Parsed(
  lb   = :Int
  name = :T
  ub   = :Number
)
julia> parse_expr(parser, :(T <: Number))
EP.TypeRange_Parsed(
  lb   = Union{}
  name = :T
  ub   = :Number
)
julia> parse_expr(parser, :(Int <: 4)) # CAUTION: when using single `<:`, the order is decisive!
EP.TypeRange_Parsed(
  lb   = Union{}
  name = :Int
  ub   = 4
)
julia> parse_expr(parser, :(4 >: Int))
ERROR: ParseError: Expected type `Symbol`, got `4` of type `Int64`.
julia> parse_expr(parser, :(4 <: Int))
ERROR: ParseError: Expected type `Symbol`, got `4` of type `Int64`.
```
"""
@exprparser struct TypeRange
  # naming is analog to Base.TypeVar
  lb = anything = Union{}
  name = anything
  ub = anything = Any
end

parse_expr(parser::TypeRange, expr::Base.Symbol) = parse_expr(parser, lb = Union{}, name = expr, ub = Any)
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
    EP.TypeAnnotation(name = EP.anything, type = EP.anything)

Parses the following
```julia
a
a::B
::B
```

# Examples
```jldoctest
julia> using ExprParsers

julia> parser = EP.TypeAnnotation(type = :Int)
EP.TypeAnnotation(
  name = ExprParsers.Isa{Any}()
  type = :Int
)
julia> parse_expr(parser, :(::Int))
EP.TypeAnnotation_Parsed(
  name = nothing
  type = :Int
)
julia> parse_expr(parser, :(a::Int))
EP.TypeAnnotation_Parsed(
  name = :a
  type = :Int
)
julia> parse_expr(parser, :(a::String))
ERROR: ParseError: Using default `==` comparison, but parser `:Int` ≠ value `:String`.
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

@doc raw"""
    EP.Arg(name = EP.anything, type = EP.anything, default = EP.anything)

A missing default value is indicated by the special variable `EP.nodefault` which is the unique instance of the
singleton type `EP.NoDefault`.

Parses the following
```julia
a
a::B
::B
a = c  # only :($(Expr(:kw, :a, :c))), not plain :(a = c)
a::B = c  # only :($(Expr(:kw, :(a::B), :c))), not plain :(a::B = c)
```

# Examples

```jldoctest
julia> using ExprParsers

julia> parser = EP.Arg()
EP.Arg(
  name    = ExprParsers.Isa{Any}()
  type    = ExprParsers.Isa{Any}()
  default = ExprParsers.Isa{Any}()
)
julia> parse_expr(parser, :(a::B))
EP.Arg_Parsed(
  name    = :a
  type    = :B
  default = ExprParsers.NoDefault()
)
julia> parse_expr(parser, Expr(:kw, :a, 3))
EP.Arg_Parsed(
  name    = :a
  type    = Any
  default = 3
)
julia> parse_expr(parser, :(a = 3))
ERROR: ParseError: AnyOf could not parse expr `a = 3` with any of the parsers `(ExprParsers.Isa{Symbol}(), EP.TypeAnnotation(name=ExprParsers.Isa{Any}(), type=ExprParsers.Isa{Any}()))`. Arg should either be a Symbol or a TypeAnnotation.
julia> parse_expr(parser, :(f(a)))
ERROR: ParseError: AnyOf could not parse expr `f(a)` with any of the parsers `(ExprParsers.Isa{Symbol}(), EP.TypeAnnotation(name=ExprParsers.Isa{Any}(), type=ExprParsers.Isa{Any}()))`. Arg should either be a Symbol or a TypeAnnotation.
```
"""
@exprparser struct Arg
  # name == nothing will be treated as anonymous type annotation
  name = anything
  type = anything = Any
  default = anything = nodefault
end

function parse_expr(parser::Arg, expr::Base.Symbol)
  parse_expr(parser, name = expr, type = Any, default = nodefault)
end

const _arg_left_handside_parser = AnyOf(anysymbol, TypeAnnotation(),
                                        errormessage = "Arg should either be a Symbol or a TypeAnnotation.")
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
    parse_expr(parser, name = name, type = type, default = nodefault)
  end
end

to_expr(parsed::Arg_Parsed) = _to_expr_Arg(parsed.name, parsed.type, parsed.default)
_to_expr_Arg(::Nothing, ::Base.Type{Any}, ::NoDefault) = :(::Any)
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
