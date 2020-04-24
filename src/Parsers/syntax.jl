using StructEquality

const suffix = "_Parsed"
"""
transforms
```
@parserfactory struct MySymbol
  symbol = anything
end
```
to
```
StructEquality.@def_structequal Base.@kwdef struct MySymbol <: ExprParsers.Parsers.Parser
  symbol = anything
end
StructEquality.@def_structequal Base.@kwdef mutable struct MySymbol_Parsed{T} <: ExprParsers.Parsers.Parsed
  symbol
end
ExprParsers.Parsed(::Base.Type{MySymbol}) = MySymbol_Parsed
```

Additionally, the created MySymbol Parser supports the following parsing syntax
```
parser = MySymbol()
parser(symbol = :hi)
```
translates to
```
parser = MySymbol()
MySymbol_Parsed(symbol = match(parser.symbol, :hi))
```
"""
# TODO performance improvement?: add typeparameters for every field (?)
macro parserfactory(struct_expr)
  if struct_expr.head == :block
    struct_expr = struct_expr.args[1]
  end

  @assert struct_expr.head == :struct "expecting struct"
  @assert struct_expr.args[1] == false "expecting immutable struct as parser"
  struct_name::Base.Symbol = _get_struct_name(struct_expr)
  mutable_struct_name = Base.Symbol(struct_name, suffix)

  mutable_struct_expr = _parsed_struct_from_parser_struct(struct_expr, mutable_struct_name)
  @assert !isa(struct_expr.args[2], Base.Expr) || struct_expr.args[2].head != :(<:) "please ommit the inheritance notation, it will be provided by the macro"

  # add inheritance
  struct_expr.args[2] = :($(struct_expr.args[2]) <: ExprParsers.Parsers.Parser)
  mutable_struct_expr.args[2] = :($(mutable_struct_expr.args[2]) <: ExprParsers.Parsers.Parsed)

  # TODO Parsed(..) calls also for typeparameters?
  esc(quote
    ExprParsers.Parsers.StructEquality.@def_structequal Base.@kwdef $struct_expr
    ExprParsers.Parsers.StructEquality.@def_structequal Base.@kwdef $mutable_struct_expr
    ExprParsers.Parsers.Parsed(::Base.Type{<:$struct_name}) = $mutable_struct_name

    # this won't be needed when function syntax for abstract types can be overloaded
    # should be there in julia 1.3.0 https://github.com/JuliaLang/julia/pull/31916
    function (parser::$struct_name)(;kw...)
      names = kw.itr  # kw isa Base.Iterators.Pairs
      matches = [ExprParsers.match(Base.getproperty(parser, name), value) for (name, value) in kw]
      kw′ = Base.NamedTuple{names}(matches)
      $mutable_struct_name(;kw′...)
    end
    # also define a default clause
    function (parser::$struct_name)(any::A) where A
      throw(ParseError("$($struct_name) has no clause defined to capture Base.Type '$A'. Got: $any"))
    end
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
- adds suffix to struct name
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


# TODO overwrite show representation to show keywords instead of position struct args

#=
# TODO will be in julia 1.3.0 https://github.com/JuliaLang/julia/pull/31916
# for now implement within macro
"""
the default implementation of calling a parser:
    match every key and return results as Parsed
"""
function (parser::P)(;kw...) where P <: Parser
  names = propertynames(kw)
  matches = [match(getproperty(parser, name), value) for (name, value) in zip(names, kw)]
  kw′ = NamedTuple{names}(matches)
  Parsed(P)(;kw′...)
end
=#
