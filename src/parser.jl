# Generic Parsers
# ===============

"""
parses:
  a
"""
@parserfactory struct SymbolParser{T}
  symbol::T
end

(p::SymbolParser)(symbol) = SymbolParser_ValuesParsed(p, symbol = symbol)
toAST(p::SymbolParser_ValuesParsed) = toAST(p.symbol)

const SingletonParser = SymbolParser
const SingletonParser_ValuesParsed = SymbolParser_ValuesParsed

"""
parses:
  Expr(:head, args...)
"""
@parserfactory struct ExprParser
  head
  args
end

(p::ExprParser)(expr::Expr) = ExprParser_ValuesParsed(p, head = expr.head, args = expr.args)
toAST(p::ExprParser_ValuesParsed) = Expr(toAST(p.head), toAST(p.args)...)



# Specific Parser
# ===============

"""
parses:
  a
  a{b, c}
"""
@parserfactory struct ReferenceExprParser{Name, Curlies}
  name::Name = anything
  curlies::Curlies = anything
end
  
(p::ReferenceExprParser)(e::Symbol) = ReferenceExprParser_ValuesParsed(p, name = e, curlies = [])

function (p::ReferenceExprParser)(e::Expr)
  @assert e.head == :curly
  name, curlies = e.args[1], e.args[2:end]
  
  ReferenceExprParser_ValuesParsed(p, name = name, curlies = curlies)
end

function toAST(p::ReferenceExprParser_ValuesParsed)
  if isempty(p.curlies)
    toAST(p.name)
  else
    :( $(toAST(p.name)){$(toAST(p.curlies)...)} )
  end
end


"""
parses:
  a
  a{b, c}
  a{d} where d
"""
@parserfactory struct TypeExprParser
  name = anything
  curlies = anything
  wheres = anything
end

(p::TypeExprParser)(e::Symbol) = TypeExprParser_ValuesParsed(p, name = e, curlies = [], wheres = [])

function (p::TypeExprParser)(e::Expr)
  e′, wheres = split_where(e)
  ref = ReferenceExprParser()(e′)
  TypeExprParser_ValuesParsed(p, name = ref.name, curlies = ref.curlies, wheres = wheres)
end

function toAST(p::TypeExprParser_ValuesParsed)
  if isempty(p.curlies)
    :( $(toAST(p.name)) where {$(toAST(p.wheres))} )
  else
    :( $(toAST(p.name)){$(toAST(p.curlies)...)} where {$(toAST(p.wheres)...)} )
  end
end


"""
parses:
  a()
  a(b, c)
  a{b, c}(d, e)
"""
@parserfactory struct CallExprParser
  name = anything
  curlies = anything
  args = anything
end

function (p::CallExprParser)(e::Expr)
  @assert e.head == :call
  called = ReferenceExprParser()(e.args[1])
  args = e.args[2:end]
  CallExprParser_ValuesParsed(p, name = called.name, curlies = called.curlies, args = args)
end

function toAST(p::CallExprParser_ValuesParsed)
  if isempty(p.curlies)
    # the comma "," is decisive!
    :( $(toAST(p.name))($(toAST(p.args)...),) )
  else
    # the comma "," is decisive!
    :( $(toAST(p.name)){$(toAST(p.curlies)...)}($(toAST(p.args)...),) )
  end
end


"""
parses:
  a
  a::B
  ::B
"""
@parserfactory struct TypeAnnotationParser
  # name == nothing will be treated as anonymous type annotation
  name = anything
  type = anything
end

(p::TypeAnnotationParser)(e::Symbol) = TypeAnnotationParser_ValuesParsed(p, name = e, type = Any)

function (p::TypeAnnotationParser)(e::Expr)
  @assert e.head == :(::)
  @assert length(e.args) <= 2
  if length(e.args) == 1
    TypeAnnotationParser_ValuesParsed(p, name = nothing, type = e.args[1])
  else
    TypeAnnotationParser_ValuesParsed(name = e.args[1], type = e.args[2])
  end 
end
function toAST(p::TypeAnnotationParser_ValuesParsed)
  if isnothing(p.name)
    :(::$(toAST(p.type)))
  else
    :($(toAST(p.name))::$(toAST(p.type)))
  end
end


struct NoDefault end
const nodefault = NoDefault()

"""
parses:
  a
  a::B
  ::B
  a = c
  a::B = c
"""
@parserfactory struct ArgParser
  # name == nothing will be treated as anonymous type annotation
  name = anything
  type = anything
  default = anything
end

(p::ArgParser)(e::Symbol) = ArgParser_ValuesParsed(p, name = e, type = Any, default = nodefault)

function (p::ArgParser)(e::Expr)
  if e.head == :(=)
    typeannotation = TypeAnnotationParser()(e.args[1])
    @assert !isnothing(typeannotation.name) "need variable name if default is given"
    ArgParser_ValuesParsed(p, name = typeannotation.name, type = typeannotation.type, default = e.args[2])
  else
    typeannotation = TypeAnnotationParser()(e)
    ArgParser_ValuesParsed(p, name = typeannotation.name, type = typeannotation.type, default = nodefault)
  end
end

function toAST(p::ArgParser_ValuesParsed)
  if isnothing(p.name) && p.default isa NoDefault
    :(::$(toAST(p.type)))
  elseif !isnothing(p.name)
    if p.default isa NoDefault
      :($(toAST(p.name))::$(toAST(p.type)))
    else
      :($(toAST(p.name))::$(toAST(p.type)) = $(toAST(p.default)))
    end
  else
    error("illegal combination encountered: p.name = $(p.name)::$(typeof(p.name)), p.default = $(p.default)::$(typeof(p.default))")
  end
end

"""
parses:
  function a(b, c)
    d
  end
  a(b, c) = d

"""
@parserfactory struct FunctionParser
  name = anything
  curlies = anything
  args = anything
  wheres = anything
  body = anything
end

function (p::FunctionParser)(expr::Expr)
  @assert expr.head in (:function, :(=))
  @assert length(expr.args) == 2
  body = expr.args[2]
  
  # we don't reuse ParsedCallExpr because functions can actually be anonymous
  call::Expr, wheres = split_where(expr.args[1])
  @assert call.head in (:tuple, :call)
  isanonymous = call.head == :tuple
  
  name, curlies, args = if isanonymous
    nothing, [], call.args
  else
    called = ParsedReferenceExpr(call.args[1])
    args = call.args[2:end]
    called.name, called.curlies, args
  end

  FunctionParser_ValuesParsed(p, name = name, curlies = curlies, args = args, wheres = wheres, body = body)
end


function toAST(p::FunctionParser_ValuesParsed)
  if isnothing(p.name)
    quote
      function ($(toAST(p.args)...),) where {$(toAST(p.wheres)...)}
        $(toAST(p.body))
      end
    end
  else
    quote
      function $(toAST(p.name))($(toAST(p.args)...),) where {$(toAST(p.wheres)...)}
        $(toAST(p.body))
      end
    end
  end
end
