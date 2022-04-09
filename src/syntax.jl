using StructEquality

const SUFFIX = "_Parsed"
"""
```julia
EP.@exprparser struct MySymbol
  symbol = EP.anything = :default_parsed_value
end
```
is transformed to
```julia
Base.@kwdef struct MySymbol <: EP.ExprParserWithParsed
  symbol = anything
end
Base.@kwdef mutable struct MySymbol_Parsed{T} <: EP.ExprParsed
  symbol = :default_parsed_value
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


Additionally, the created MySymbol Parser supports the following default `parse_expr` functionality
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
macro exprparser(combined_struct_expr)
  # TODO performance improvement?: add typeparameters for every field (?)
  if combined_struct_expr.head == :block
    combined_struct_expr = combined_struct_expr.args[1]
  end
  @assert combined_struct_expr.head == :struct "expecting struct"
  @assert combined_struct_expr.args[1] == false "expecting immutable struct as parser"
  @assert(!isa(combined_struct_expr.args[2], Base.Expr) || combined_struct_expr.args[2].head != :(<:),
    "please ommit the inheritance notation, it will be provided by the macro")

  parser_struct_name::Base.Symbol = _get_struct_name(combined_struct_expr)
  parser_struct_expr = _parser_struct_from_combined_struct(combined_struct_expr, parser_struct_name)

  parsed_struct_name = Base.Symbol(parser_struct_name, SUFFIX)
  parsed_struct_expr = _parsed_struct_from_combined_struct(combined_struct_expr, parsed_struct_name)

  # add inheritance
  parser_struct_expr.args[2] = :($(parser_struct_expr.args[2]) <: ExprParsers.ExprParserWithParsed)
  parsed_struct_expr.args[2] = :($(parsed_struct_expr.args[2]) <: ExprParsers.ExprParsed)

  # TODO Parsed(..) calls also for typeparameters?
  esc(quote
    ExprParsers.StructEquality.@struct_hash_equal Base.@kwdef $parser_struct_expr
    ExprParsers.StructEquality.@struct_hash_equal Base.@kwdef $parsed_struct_expr
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


_rename_struct_name(newname, expr::Base.Expr) = _rename_struct_name(newname, Val{expr.head}(), expr.args)
_rename_struct_name(newname, symbol::Base.Symbol) = newname
_rename_struct_name(newname, any) = any
_rename_struct_name(newname, head::Union{Val{:(<:)}, Val{:(::)}, Val{:curly}}, args) = Base.Expr(get(head), _rename_struct_name(newname, args[1]), args[2:end]...)
_rename_struct_name(newname, ::Val{:struct}, args) = _rename_struct_name(newname, args[2])

"""
- makes it mutable
- adds SUFFIX to struct name
- deletes all default values
"""
function _parser_struct_from_combined_struct(struct_expr::Base.Expr, newname)
  @assert struct_expr.head == :struct

  # take first default value and ommit a possible second one
  _default(expr::Base.Expr) = _default(expr, Val{expr.head}(), expr.args)
  _default(any) = any
  _default(expr, head::Val{:block}, args) = Base.Expr(:block, _default.(args)...)
  _default(expr, head::Val{:(=)}, args) = _default_assignment(expr, args[1], args[2])
  _default_assignment(expr, a, b) = expr # if there is no double assignment, but hence only a single, we take the single
  _default_assignment(expr, a, b::Base.Expr) = _default_assignment(expr, a, Val(b.head), b.args)
  _default_assignment(expr, a, head::Val{:(=)}, bargs) = :($a = $(bargs[1]))  # if there are two assignments, take the first
  _default_assignment(expr, a, head, bargs) = expr  # if there is only one assignment, take it

  Base.Expr(:struct, true, _rename_struct_name(newname, struct_expr.args[2]), _default(struct_expr.args[3]))
end

"""
- makes it mutable
- adds SUFFIX to struct name
- deletes all default values
"""
function _parsed_struct_from_combined_struct(struct_expr::Base.Expr, newname)
  @assert struct_expr.head == :struct

  # take first default value and ommit a possible second one
  _default(expr::Base.Expr) = _default(expr, Val{expr.head}(), expr.args)
  _default(any) = any
  _default(expr, head::Val{:block}, args) = Base.Expr(:block, _default.(args)...)
  _default(expr, head::Val{:(=)}, args) = _default_assignment(expr, args[1], args[2])
  _default_assignment(expr, a, b) = a # if there is no double assignment, but hence only a single, we take none
  _default_assignment(expr, a, b::Base.Expr) = _default_assignment(expr, a, Val(b.head), b.args)
  _default_assignment(expr, a, head::Val{:(=)}, bargs) = :($a = $(bargs[2]))  # if there are two assignments, take the second
  _default_assignment(expr, a, head, bargs) = a  # if there is only one assignment, we take none

  Base.Expr(:struct, true, _rename_struct_name(newname, struct_expr.args[2]), _default(struct_expr.args[3]))
end
