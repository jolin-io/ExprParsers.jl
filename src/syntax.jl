#= TODO support syntax like

=#

"""
transforms
```
@parserfactory struct SymbolParser{T}
  symbol::T
end
```
to
```
Base.@kwdef struct SymbolParser{T} <: ASTParser.Parser
  symbol::T
end
Base.@kwdef mutable struct SymbolParser_ValuesParsed{T}
  symbol::T
end
```

Additionally, the created SymbolParser_ValuesParsed supports the following syntax
```
function (p::SymbolParser)(expr)
  ...
  SymbolParser_ValuesParsed(p, symbol = symbol)
end
```
translates to
```
function (p::SymbolParser)(expr)
  ...
  SymbolParser_ValuesParsed(
    symbol = match(p.symbol = symbol),
  )
end
```
"""
macro parserfactory(struct_expr)
  suffix = "_ValuesParsed"
  if struct_expr.head == :block
    struct_expr = struct_expr.args[1]
  
  @assert struct_expr.head == :struct "expecting struct"
  @assert struct_expr.args[1] == false "expecting immutable struct as parser"
  struct_name::Symbol = get_name(struct_expr)
  mutable_struct_name = Symbol(struct_name, suffix)

  mutable_struct_expr = deepcopy(struct_expr)
  mutable_struct_expr.args[1] = true
  change_name!(mutable_struct_expr) do old_name
    Symbol(old_name, suffix)
  end

  @assert struct_expr.args[2].head != :(<:) "please ommit the inheritance notation, it will be provided by the macro"
    
  # add inheritance
  # caution: we only want to add it to the immutable struct, hence the deepcopy must happen before
  struct_expr.args[2] = :($(struct_expr.args[2]) <: ASTParser.Parser)
  
  quote
    Base.@kwdef $(esc(struct_expr))
    Base.@kwdef $(esc(mutable_struct_expr))
    function ($(esc(mutable_struct_name)))(parser::ASTParser.Parser; kw...)
      names = propertynames(kw)
      matches = [match(getproperty(parser, name), value) for (name, value) in zip(names, kw)]
      kw′ = NamedTuple{names}(matches)
      $(esc(mutable_struct_name))(;kw′...)
    end
  end
end