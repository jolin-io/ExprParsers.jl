# Simple Parsers
# ==============

"""
    EP.SatisfiesPredicate(predicate_func[, errormessage])

Construct an ExprParser which checks whether the given `predicate_func` returns true.
If so, the to-be-parsed value is returned as such, otherwise an [`ParseError`](@ref) is thrown as usual.
If `errormessage` is given, it will be appended to the default error message.

# Example
```jldoctest
julia> using ExprParsers;

julia> parser = EP.SatisfiesPredicate(isodd);

julia> parse_expr(parser, 3)
3
julia> parse_expr(parser, 4)
ERROR: ParseError: Predicate `isodd` returned false on expr `4`.
julia> is44(x) = x==44;

julia> parser2 = EP.SatisfiesPredicate(is44, "It should be 44.");

julia> parse_expr(parser2, 44)
44
julia> parse_expr(parser2, 4)
ERROR: ParseError: Predicate `is44` returned false on expr `4`. It should be 44.
```
"""
struct SatisfiesPredicate{Func} <: ExprParser
  predicate::Func
  errormessage
end
SatisfiesPredicate(func) = SatisfiesPredicate(func, "")
function parse_expr(parser::SatisfiesPredicate, expr)
  parser.predicate(expr) ? expr : throw(ParseError(
    "Predicate `$(parser.predicate)` returned false on expr `$expr`. $(parser.errormessage)"
  ))
end

"""
    EP.Isa(T::Type)

Constructs an ExprParser which checks whether a value is of the given type.

# Example
```jldoctest
julia> using ExprParsers

julia> parser = EP.Isa(Symbol);

julia> parse_expr(parser, :thisisasymbol)
:thisisasymbol
julia> parse_expr(parser, 42)
ERROR: ParseError: Expected type `Symbol`, got `42` of type `Int64`.
```

`EP.Isa(Symbol)` is so common that there is a special constant for it [`anysymbol`](@ref).
"""
struct Isa{T} <: ExprParser
  # this way we cannot forget to instantiate the Type
  Isa(::Base.Type{T}) where T = new{T}()
end
parse_expr(parser::Isa{T}, expr::T) where T = expr
parse_expr(parser::Isa{T}, other::S) where {T, S} = throw(ParseError("Expected type `$T`, got `$other` of type `$S`."))

"""
    EP.anything = Isa(Any)

Special constant ExprParser which matches literally anything.

# Examples
```jldoctest
julia> using ExprParsers

julia> parse_expr(EP.anything, 42)
42
julia> parse_expr(EP.anything, :whatever)
:whatever
```
"""
const anything = Isa(Any)

"""
    EP.anysymbol = Isa(Symbol)

Special constant ExprParser which matches Symbols, and only Symbols.

# Examples
```jldoctest
julia> using ExprParsers

julia> parse_expr(EP.anysymbol, :asymbol)
:asymbol
julia> parse_expr(EP.anysymbol, 42)
ERROR: ParseError: Expected type `Symbol`, got `42` of type `Int64`.
```
"""
const anysymbol = Isa(Symbol)

"""
    EP.AnyOf(parser1, parser2, parser3, ...; errormessage = "")

Constructs an ExprParser from multiple given parsers. When given a value it first tries to match
`parser1`, and if that fails with a ParseError, then `parser2`, and so forth.
The result from the first parser which matches will be returned.
If no parser matches, a dedicated ParseError is raised.

If `errormessage` is given, it will be appended to the default error message in case of ParseError.

# Examples
```jldoctest
julia> using ExprParsers

julia> parser = EP.AnyOf(EP.anysymbol, EP.Isa(String), EP.SatisfiesPredicate(isodd),
                         errormessage="My error message.");

julia> parse_expr(parser, :hi)
:hi
julia> parse_expr(parser, 3)
3
julia> parse_expr(parser, "something")
"something"
julia> parse_expr(parser, 4)
ERROR: ParseError: AnyOf could not parse expr `4` with any of the parsers `(ExprParsers.Isa{Symbol}(), ExprParsers.Isa{String}(), ExprParsers.SatisfiesPredicate{typeof(isodd)}(isodd, ""))`. My error message.
```
"""
struct AnyOf{T} <: ExprParser
  several::T
  errormessage
  AnyOf(several...; errormessage = "") = new{typeof(several)}(several, errormessage)
end

function parse_expr(parser::AnyOf, expr)
  for subparser in parser.several
    try
      return parse_expr(subparser, expr)
    catch e
      # only continue if ParseError
      e isa ParseError || rethrow()
    end
  end
  throw(ParseError("AnyOf could not parse expr `$(expr)` with any of the parsers `$(parser.several)`. $(parser.errormessage)"))
end

"""
    EP.AllOf(parser1, parser2, parser3, ...)

Constructs an ExprParser from multiple given parsers. When to match a value, all parsers actually need to
parse correctly, otherwise the `ParseError` from the first non-matching parser is rethrown.
If all parsers match, then the return value from the last parser is returned.

# Examples
```jldoctest
julia> using ExprParsers

julia> parser = EP.AllOf(EP.Isa(Number), EP.SatisfiesPredicate(isodd), 3);

julia> parse_expr(parser, 3)
3
julia> parse_expr(parser, "something")
ERROR: ParseError: Expected type `Number`, got `something` of type `String`.
julia> parse_expr(parser, 4)
ERROR: ParseError: Predicate `isodd` returned false on expr `4`.
julia> parse_expr(parser, 5)
ERROR: ParseError: Using default `==` comparison, but parser `3` ≠ value `5`.
```
"""
struct AllOf{T} <: ExprParser
  several::T
  AllOf(several...) = new{typeof(several)}(several)
end

function parse_expr(parser::AllOf, expr)
  for subparser in parser.several[1:end-1]
    parse_expr(subparser, expr)
  end
  # return last match, if all else matched
  parse_expr(parser.several[end], expr)
end
