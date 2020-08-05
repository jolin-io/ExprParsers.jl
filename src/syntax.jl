using StructEquality

const SUFFIX = "_Parsed"
"""
```julia
EP.@exprparser struct MySymbol
  symbol = anything
end
```
is transformed to
```julia
StructEquality.@def_structequal Base.@kwdef struct MySymbol <: EP.ExprParserWithParsed
  symbol = anything
end
StructEquality.@def_structequal Base.@kwdef mutable struct MySymbol_Parsed{T} <: EP.ExprParsed
  symbol
end
EP.ExprParsed(::Base.Type{MySymbol}) = MySymbol_Parsed
```

It defines the basics for an usual ExprParser, namely
- the Parser type itself like specified in the original struct. It is always immutable - if you feel the need of
  mutating a parser, try to construct a new parser instead.
- a corresponding Parsed type which will be used to hold parsed values. This is intentionally mutable as a usual
  workflow consists of adapting the parsed values to the needs of your macro, and if everything is changed, transform
  it back to an Expr using `to_expr(parsed)`.
- a mapping from the `ExprParser` to the `ExprParsed`


Additionally, the created MySymbol Parser supports the following parsing syntax
```julia
parser = MySymbol()
parse_expr(parser, symbol = :hi)
```
which translates to
```julia
parser = MySymbol()
MySymbol_Parsed(symbol = parse_expr(parser.symbol, :hi))
```
This is generic, and works similar if you have multiple fields.

----------------

Finally, in order to finish your custom ExprParser definition, you just need to specialize the two main functions
- `parse_expr(mysymbolparser::MySymbol, expr)`
- `to_expr(mysymbolparsed::MySymbol_Parsed)`

```julia
function EP.parse_expr(mysymbolparser::MySymbol, expr)
  # do your custom parsing
  # use @passert for checking parse assertions (it will have a nice and detailed default error message)
  # construct your parsed result
  MySymbol_Parsed(symbol = ...)
end

function EP.to_expr(parsed::MySymbol_Parsed)
  # create a proper `Base.Expr` from your parsed result
  # in this case it is simple
  parsed.symbol
end
```
"""
# TODO performance improvement?: add typeparameters for every field (?)
macro exprparser(parser_struct_expr)
  if parser_struct_expr.head == :block
    parser_struct_expr = parser_struct_expr.args[1]
  end

  @assert parser_struct_expr.head == :struct "expecting struct"
  @assert parser_struct_expr.args[1] == false "expecting immutable struct as parser"
  parser_struct_name::Base.Symbol = _get_struct_name(parser_struct_expr)
  parsed_struct_name = Base.Symbol(parser_struct_name, SUFFIX)

  parsed_struct_expr = _parsed_struct_from_parser_struct(parser_struct_expr, parsed_struct_name)
  @assert(!isa(parser_struct_expr.args[2], Base.Expr) || parser_struct_expr.args[2].head != :(<:),
    "please ommit the inheritance notation, it will be provided by the macro")

  # add inheritance
  parser_struct_expr.args[2] = :($(parser_struct_expr.args[2]) <: ExprParsers.ExprParserWithParsed)
  parsed_struct_expr.args[2] = :($(parsed_struct_expr.args[2]) <: ExprParsers.ExprParsed)

  # TODO Parsed(..) calls also for typeparameters?
  esc(quote
    ExprParsers.StructEquality.@def_structequal Base.@kwdef $parser_struct_expr
    ExprParsers.StructEquality.@def_structequal Base.@kwdef $parsed_struct_expr
    ExprParsers.ExprParsed(::Base.Type{<:$parser_struct_name}) = $parsed_struct_name
  end)
end

"""
  returns name of struct definition as symbol
"""
_get_struct_name(expr::Base.Expr) = _get_struct_name(Val{expr.head}(), expr.args)
_get_struct_name(::Val{:struct}, args) = _get_struct_name(args[2])
_get_struct_name(::Union{Val{:(<:)}, Val{:(::)}, Val{:curly}}, args) = _get_struct_name(args[1])
_get_struct_name(symbol::Base.Symbol) = symbol

"""
- makes it mutable
- adds SUFFIX to struct name
- deletes all default values
"""
function _parsed_struct_from_parser_struct(struct_expr::Base.Expr, newname)
  @assert struct_expr.head == :struct

  _h(expr::Base.Expr) = _h(Val{expr.head}(), expr.args)
  _h(::Val{:struct}, args) = Base.Expr(:struct, true, _h_name(args[2]), _h_default(args[3]))
  # rename struct name with given newname
  _h_name(expr::Base.Expr) = _h_name(Val{expr.head}(), expr.args)
  _h_name(symbol::Base.Symbol) = newname
  _h_name(any) = any
  _h_name(head::Union{Val{:(<:)}, Val{:(::)}, Val{:curly}}, args) = Base.Expr(get(head), _h_name(args[1]), args[2:end]...)
  # delete default arguments
  _h_default(expr::Base.Expr) = _h_default(expr, Val{expr.head}(), expr.args)
  _h_default(any) = any
  _h_default(expr, head::Val{:block}, args) = Base.Expr(:block, _h_default.(args)...)
  _h_default(expr, head::Val{:(=)}, args) = args[1] isa Base.Symbol ? args[1] : expr

  _h(struct_expr)
end
