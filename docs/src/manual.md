Manual
======

The main interface encompass just three concepts, which seamlessly interact with oneanother
- macro [`@exprparser`](@ref): easily create definitions for highly flexible and nestable parsers
- function [`parse_expr`](@ref): compares a matcher with a value, and returns a parsed value
- function [`to_expr`](@ref): transforms parsed values back to AbstractSyntaxTrees

TBD


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
