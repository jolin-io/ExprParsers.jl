# Simple Parsers
# ==============

abstract type SimpleParser end

struct SatisfiesPredicate{Func} <: SimpleParser
  predicate::Func
end
function parse_expr(parser::SatisfiesPredicate, expr)
  parser.predicate(expr) ? expr : throw(ParseError("Predicate $parser returned false on expr $expr."))
end

struct Isa{T} <: SimpleParser
  # this way we cannot forget to instantiate the Type
  Isa(::Base.Type{T}) where T = new{T}()
end
parse_expr(parser::Isa{T}, expr::T) where T = expr
parse_expr(parser::Isa{T}, other::S) where {T, S} = throw(ParseError("Expected type $T, got $other of type $S."))

const anything = Isa(Any)
const anysymbol = Isa(Symbol)

struct AnyOf{T} <: SimpleParser
  several::T
  AnyOf(several...) = new{typeof(several)}(several)
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
  throw(ParseError("could not parse any"))
end

struct AllOf{T} <: SimpleParser
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
