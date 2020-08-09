```@meta
CurrentModule = ExprParsers
```

# Public API

## Interface

- [`EP`](@ref)
- [`parse_expr`](@ref)
- [`to_expr`](@ref)
- [`@passert`](@ref)
- [`ParseError`](@ref)

```@docs
EP
parse_expr
to_expr
@passert
ParseError
```

## Types

- [`ExprParser`](@ref)
- [`ExprParserWithParsed`](@ref)
- [`ExprParsed`](@ref)
- [`@exprparser`](@ref)

```@docs
ExprParser
ExprParserWithParsed
ExprParsed
@exprparser
```

## Core ExprParser

- [`ExprParsers.Utils.Iterator`](@ref)
- [`SatisfiesPredicate`](@ref)
- [`Isa`](@ref)
- [`anything`](@ref)
- [`anysymbol`](@ref)
- [`AnyOf`](@ref)
- [`AllOf`](@ref)

```@docs
ExprParsers.Utils.Iterator
SatisfiesPredicate
Isa
anything
anysymbol
AnyOf
AllOf
```

## Meta ExprParser

- [`Named`](@ref)
- [`Indexed`](@ref)

```@docs
Named
Indexed
```

## ExprParserWithParsed

All these ExprParsers have a corresponding `..._Parsed` object which captures the parsed information. I.e. there is `EP.Function` and when it is parsed with `parse_expr` it will return an `EP.Function_Parsed`.

- [`Expr`](@ref)
- [`Block`](@ref)
- [`Macro`](@ref)
- [`Assignment`](@ref)
- [`NestedDot`](@ref)
- [`Reference`](@ref)
- [`Call`](@ref)
- [`Signature`](@ref)
- [`Function`](@ref)
- [`Type`](@ref)
- [`TypeRange`](@ref)
- [`TypeAnnotation`](@ref)
- [`Arg`](@ref)

```@docs
Expr
Block
Macro
Assignment
NestedDot
Reference
Call
Signature
Function
Type
TypeRange
TypeAnnotation
Arg
```
