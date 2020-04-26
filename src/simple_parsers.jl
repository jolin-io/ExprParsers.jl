# Simple Parsers
# ==============

abstract type SimpleParser end

struct SatisfiesPredicate{Func} <: SimpleParser
  predicate::Func
end
parse_expr(m::SatisfiesPredicate, a) = m.predicate(a) ? a : throw(ParseError("Predicate $m returned false."))

struct Isa{T} <: SimpleParser
  # this way we cannot forget to instantiate the Type
  Isa(::Base.Type{T}) where T = new{T}()
end
parse_expr(m::Isa{T}, a::T) where T = a
parse_expr(m::Isa{T}, ::S) where {T, S} = throw(ParseError("Expected type $T, found type $S."))

const anything = Isa(Any)


struct AnyOf{T} <: SimpleParser
  several::T
  AnyOf(several...) = new{typeof(several)}(several)
end

function parse_expr(m::AnyOf, a)
  for x in m.several
    try
      return parse_expr(x, a)
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

function parse_expr(m::AllOf, a)
  for x in m.several[1:end-1]
    parse_expr(x, a)
  end
  # return last match, if all else matched
  parse_expr(m.several[end], a)
end
