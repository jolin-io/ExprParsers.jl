Manual
======

`ExprParsers` exports a constant `EP` which is an alias for the package `ExprParsers` itself. This comes in very handy when you use the custom parsers a lot.

The main interface encompass just three concepts, which seamlessly interact with oneanother
- macro [`EP.@exprparser`](@ref @exprparser): easily create definitions for highly flexible and nestable parsers
- function [`parse_expr`](@ref): compares a parser with a value, and returns a parsed result
- function [`to_expr`](@ref): transforms parsed values back to `Base.Expr`
