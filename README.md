# ExprParsers.jl

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://schlichtanders.github.io/ExprParsers.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://schlichtanders.github.io/ExprParsers.jl/dev)
[![Build Status](https://github.com/schlichtanders/ExprParsers.jl/workflows/CI/badge.svg)](https://github.com/schlichtanders/ExprParsers.jl/actions)
[![Coverage](https://codecov.io/gh/schlichtanders/ExprParsers.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/schlichtanders/ExprParsers.jl)


`ExprParsers` is a library made to simplify development of elaborate macros.

What `ExprParsers` offers is a set of curated parsers for common `Expr` patterns. For example `parse_expr(ExprParsers.Function(), :(f(a) = 2a))` will give you a `ExprParsers.Function_Parsed` object where you can inspect and change `name`, `args`, `kwargs`, `curlies`, `wheres`, and the function `body`. It just works and you don't have to bother any longer that you can also write the same function as `function f(a); 2a; end` - the parser handles this for you.

In macros you often not only want to inspect the given `Expr` in efficient and stable manners, but also may want to change parts and return a respectively adapted `Expr`. For this purpose, all Parsed objects can be converted back to `Expr` by using the `to_expr()` method.

We guarantee that `parse_expr` and `to_expr` are working nicely together, i.e. the following always holds for arbitrary expressions and parsers
```julia
using ExprParsers  # comes with a shorthand EP for ExprParsers
parser = EP.Function()
expr = :(f(a) = 2a))
parsed = parse_expr(parser, expr)

# applying the parser "twice" returns always the same parsed result
parse_expr(parser, to_expr(parsed)) == parsed
```

Note that `ExprParsers` exports a constant `EP` which is an alias for the package `ExprParsers` itself. This comes in very handy when you use the custom parsers a lot.

Checkout the `test/` directory for seeing more examples, especially [test/expr_parsers_with_parsed.jl](test/expr_parsers_with_parsed.jl) where for each common `Expr` pattern a parser is put into action.


## Installation

The package is soon going to be registered at General, until then you can use it by adding a custom registry.
```julia
using Pkg
pkg"registry add https://github.com/JuliaRegistries/General"  # central julia registry
pkg"registry add https://github.com/schlichtanders/SchlichtandersJuliaRegistry.jl"  # custom registry
pkg"add ExprParsers"
```

Use it like
```julia
using ExprParsers
```
