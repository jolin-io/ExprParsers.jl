```@meta
CurrentModule = ExprParsers
```

# Public API

## Interface

```@docs
parse_expr
to_expr
@passert
ParseError
```

## Types
```@docs
ExprParser
ExprParserWithParsed
ExprParsed
@exprparser
```

## Core ExprParser

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

```@docs
Named
Indexed
```

## ExprParserWithParsed

All these ExprParsers have a corresponding `..._Parsed` object which captures the parsed information. I.e. there is `EP.Function` and when it is parsed with `parse_expr` it will return an `EP.Function_Parsed`.

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
