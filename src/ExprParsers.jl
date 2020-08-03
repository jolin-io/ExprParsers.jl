"""
    ExprParsers

the main interface encompass just three concepts, which seamlessly interact with oneanother
- macro `@exprparser`: easily create definitions for highly flexible and nestable parsers
- function `parse_expr`: compares a matcher with a value, and returns a parsed value
- function `to_expr`: transforms parsed values back to AbstractSyntaxTrees
"""
module ExprParsers
export EP, @passert, ParseError,
  parse_expr, to_expr,
  @exprparser

const EP = ExprParsers

using Compat


include("Utils.jl")
using .Utils
include("exceptions.jl")
include("syntax.jl")


# Main Interface
# ==============

# TODO adapt doc string
"""
    parse_expr(parser, value)


Match parser against a value, will throw ParseError if the parser does not match.

Defaults to comparing with `==`, if matches, will return the value.
Parsers will be called instead and return their parsed value.
"""
function parse_expr(parser, value)
  @passert parser == value "Using default `==` comparison, but parser `$(repr(parser))` ≠ value `$(repr(value))`."
  value
end
# vectors are treated especially
function parse_expr(parser::Union{Vector, Tuple, Iterator}, values::Union{Vector, Tuple})
  map(parser, values) do p, v
    parse_expr(p, v)
  end
end
# also Base.Expr are treated especially
function parse_expr(parser::Base.Expr, values::Base.Expr)
  @passert parser.head == values.head
  Base.Expr(parser.head, parse_expr(parser.args, values.args)...)
end

"""
    to_expr(parsed)

Converts parsed information back to Expr.

Defaults to returning same value, however if something knows about how it can be translated back,
just overload the function.
"""
to_expr(a) = a
to_expr(a::Union{Vector, Tuple}) = map(to_expr, a)  # reverses also Iterator
to_expr(a::Base.Expr) = Base.Expr(a.head, to_expr(a.args)...)

"""
All parsers in the `ExprParsers` package inherit from this type.
"""
abstract type ExprParser end

"""
This is mainly for internal usage. Please use `@exprparser` instead for the public interface.

Subtype of `ExprParser` which indicates that this parser actually constructs a `ExprParsed` object when calling
`parse_expr`.
The resulting `ExprParsed` object is a struct with identical fields like the parser,
where then the parsed values will be stored.

`ExprParsed(parser::ExprParserWithParsed)` will return the corresponding `ExprParsed` type.
"""
abstract type ExprParserWithParsed <: ExprParser end

"""
    ExprParsed(parser::ExprParserWithParsed)::ExprParsed

Maps Parser to respective Parsed type, and is also abstract super type of all Parsed types.

Example
```
EP.ExprParsed(EP.Assignment) == EP.Assignment_Parsed
```
"""
abstract type ExprParsed end

Base.convert(::Base.Type{Base.Expr}, parsed::ExprParsed) = to_expr(parsed)


# Parsers
# =======

include("expr_parsers_core.jl")
include("expr_parsers_with_parsed.jl")
include("expr_parsers_meta.jl")

end # module