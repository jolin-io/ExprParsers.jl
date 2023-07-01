# ExprParsers.jl

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://jolin-io.github.io/ExprParsers.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://jolin-io.github.io/ExprParsers.jl/dev)
[![Build Status](https://github.com/jolin-io/ExprParsers.jl/workflows/CI/badge.svg)](https://github.com/jolin-io/ExprParsers.jl/actions)
[![Coverage](https://codecov.io/gh/jolin-io/ExprParsers.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/jolin-io/ExprParsers.jl)


`ExprParsers` is a library made to simplify development of elaborate macros.

What `ExprParsers` offers is a set of curated parsers for common `Expr` patterns. For example
```julia
parse_expr(ExprParsers.Function(), :(f(a) = 2a))
```
will give you an `ExprParsers.Function_Parsed` object where you can inspect and change `name`, `args`, `kwargs`, `curlies`, `wheres`, and the function `body`. It just works and you don't have to bother any longer that you can also write the same function as `function f(a); 2a; end` - the parser handles this for you.

In macros you often not only want to inspect the given `Expr` in efficient and stable manners, but also may want to change parts and return a respectively adapted `Expr`. For this purpose, all Parsed objects are mutable and can be converted back to `Expr` by using the `to_expr(parsed_value)` method.

We guarantee that `parse_expr` and `to_expr` are working nicely together, i.e. the following always holds for arbitrary expressions and parsers

```julia
julia> using ExprParsers

julia> # comes with a shorthand EP for ExprParsers

julia> parser = EP.Function()
EP.Function(
  name    = ExprParsers.Isa{Any}()
  curlies = ExprParsers.Isa{Any}()
  args    = ExprParsers.Isa{Any}()
  kwargs  = ExprParsers.Isa{Any}()
  wheres  = ExprParsers.Isa{Any}()
  body    = ExprParsers.Isa{Any}()
)

julia> expr = :(f(a) = 2a)
:(f(a) = begin
          #= REPL[8]:1 =#
          2a
      end)

julia> parsed = parse_expr(parser, expr)
EP.Function_Parsed(
  name    = :f
  curlies = Any[]
  args    = Any[:a]
  kwargs  = Any[]
  wheres  = Any[]
  body    = quote
    #= REPL[8]:1 =#
    2a
end
)

julia> # applying the parser "twice" returns always the same parsed result

julia> parse_expr(parser, to_expr(parsed)) == parsed
true
```

Note that `ExprParsers` exports a constant `EP` which is an alias for the package `ExprParsers` itself. This comes in very handy when you use the custom parsers a lot.

Checkout the `test/` directory for seeing more examples, especially [test/expr_parsers_with_parsed.jl](test/expr_parsers_with_parsed.jl) where for each common `Expr` pattern a parser is put into action.


## Installation

install by
```julia
using Pkg
pkg"add ExprParsers"
```