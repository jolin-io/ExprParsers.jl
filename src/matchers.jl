#=
Matchers just define objects with convenient equal methods "==" 
=#

struct MatchAnything end
const anything = MatchAnything()
Base.:(==)(::MatchAnything, _) = true
Base.:(==)(_, ::MatchAnything) = true

struct MatchAny{T}
  several::T
  MatchAny(several...) = new{typeof(several)}(several)
end
Base.:(==)(m::MatchAny, a) = a in m.several
Base.:(==)(a, m::MatchAny) = a in m.several

struct MatchAll{T}
  several::T
  MatchAll(several...) where T = new{typeof(several)}(several)
end
Base.:(==)(m::MatchAll, a) = all(x -> x == a, m.several)
Base.:(==)(a, m::MatchAll) = all(x -> x == a, m.several)
