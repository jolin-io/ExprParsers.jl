module Matchers
# import ASTParser: @passert, ParseError
import ProxyInterface
using ASTParser
export Matcher, match, anything

# Main Interface
# ==============

# Matchers overwrite ``match```
abstract type Matcher end
# TODO enable this as soon as julia 1.3.0 is out
# currently it throws "ERROR: cannot add methods to an abstract type"
# (matcher::Matcher)(expr) = match(matcher, expr)

"""
  match something against a value, defaults to ==

will throw AssertError if does not match

if matches, by default will return the value if matches
however Parsers will be called instead and return their parsed value
"""
function match(matcher, value)
  @passert matcher == value "Using default ``==`` matcher, but matcher ``$(repr(matcher))`` ≠ value ``$(repr(value))``."
  value
end

# vectors are treated especially
function match(matcher::Union{Vector, Tuple}, values::Union{Vector, Tuple})
  [match(m, v) for (m, v) in zip(matcher, values)]
end

# also Base.Expr are treated especially
function match(matcher::Base.Expr, values::Base.Expr)
  @passert matcher.head == values.head
  Base.Expr(matcher.head, match(matcher.args, values.args)...)
end

# Generic Matchers
# ================


struct Predicate{Func}
  predicate::Func
end
match(m::Predicate, a) = m.predicate(a) ? a : throw(ParseError("Predicate $m returned false."))
(m::Predicate)(a) = match(m, a)

struct Anything <: Matcher end
const anything = Anything()
match(::Anything, a) = a
(::Anything)(a) = a

struct Type{T} <: Matcher
  # this way we cannot forget to instantiate the Type
  Type(_::Base.Type{T}) where T = new{T}()
end
match(m::Type{T}, a::T) where T = a
match(m::Type{T}, _::S) where {T, S} = throw(ParseError("Expected type $T, found type $S."))


struct AnyOf{T} <: Matcher
  several::T
  AnyOf(several...) = new{typeof(several)}(several)
end

function match(m::AnyOf, a)
  for x in m.several
    try
      return match(x, a)
    catch e
      # only continue if ParseError
      e isa ParseError || rethrow()
    end
  end
  throw(ParseError("could not parse any"))
end
(m::AnyOf)(a) = match(m, a)



struct AllOf{T} <: Matcher
  several::T
  AllOf(several...) = new{typeof(several)}(several)
end

function match(m::AllOf, a)
  for x in m.several[1:end-1]
    match(x, a)
  end
  # return last match, if all else matched
  match(m.several[end], a)
end
(m::AllOf)(a) = match(m, a)


struct Iterator{T} <: Matcher
  iterator::T
end
ProxyInterface.iterator(i::Iterator) = i.iterator
ProxyInterface.iterator(::Base.Type{Iterator{T}}) where T = T
ProxyInterface.@iterator Iterator

function match(matchers::Iterator, values)
  map(matchers.iterator, values) do m, v
    match(m, v)
  end
end
(m::Iterator)(a) = match(m, a)

"""
when given several Iterator, combine them elementwise with AnyOf
"""
function Iterator(iterators::Vararg{<:Union{Iterator, Vector, Tuple}})
  Iterator(Base.Generator(AnyOf, iterators...))
end

end  # module
