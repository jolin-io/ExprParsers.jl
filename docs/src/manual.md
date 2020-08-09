Manual
======

`ExprParsers` exports a constant `EP` which is an alias for the package `ExprParsers` itself. This comes in very handy when you use the custom parsers a lot.

The main interface encompass just three concepts, which seamlessly interact with oneanother
- macro [`EP.@exprparser`](@ref @exprparser): easily create definitions for highly flexible and nestable parsers
- function [`parse_expr`](@ref): compares a parser with a value, and returns a parsed result
- function [`to_expr`](@ref): transforms parsed values back to `Base.Expr`


Many parsers have already been defined, ready for use, and well documented. Take a look at [Public API](@ref).


Extended Example: Part I Combining Parsers
------------------------------------------

To understand how to work with `ExprParsers` in practice, it is best to see a full-fledged example.

One nice and complex task would be to implement support for the traitor-like-syntax introduced by the package [`Traitor.jl`](https://github.com/andyferris/Traitor.jl). The syntax may no longer be the nicest, but still it is quite powerful and serves as a good example (I myself build another Traits package which extends where syntax instead, which many people may find more intuitive these days [`WhereTraits.jl`](https://github.com/schlichtanders/WhereTraits.jl)).

In Traitor syntax, you have a double type annotation `argument::StandardType::TraitsType`. The `StandardType` is a plain julia type like `Int` or `AbstractArray`. The `TraitsType` is something similar to `Base.HasEltype` or `Base.HasLength`. For our goal it doesn't matter so much, we just want to parse the syntax.

Let's start with loading the package and creating a default parser for functions.
```jldoctest example
julia> using ExprParsers

julia> parser_function = EP.Function(name = EP.anysymbol)
EP.Function(
  name    = ExprParsers.Isa{Symbol}()
  curlies = ExprParsers.Isa{Any}()
  args    = ExprParsers.Isa{Any}()
  kwargs  = ExprParsers.Isa{Any}()
  wheres  = ExprParsers.Isa{Any}()
  body    = ExprParsers.Isa{Any}()
)
```
As you can see, we only gave a subparser for the field `name` which captures the function name. All other substructures default to be able to parse anything.

Now we parse our first function. Mind that we need to use `:(...)` for constructing
Expr, as `quote...end` would introduce an additional `Expr(:block, ...)` layer.
```jldoctest example
julia> parsed_function = parse_expr(parser_function, :(
         function f(a, b::Int::HasSomeTrait)
            "a = $a, b = $b, sometrait(b) = $(sometrait(b))"
         end
       ))
EP.Function_Parsed(
  name    = :f
  curlies = Any[]
  args    = Any[:a, :((b::Int)::HasSomeTrait)]
  kwargs  = Any[]
  wheres  = Any[]
  body    = quote
    #= none:3 =#
    "a = $(a), b = $(b), sometrait(b) = $(sometrait(b))"
end
)
```

We have our parsed function, but the arguments are still quite rough. Lets parse them further by using `EP.Arg`. Conveniently, all ExprParsers support broadcasting syntax.

```jldoctest example
julia> parsed_args = parse_expr.(EP.Arg(), parsed_function.args)
2-element Array{ExprParsers.Arg_Parsed,1}:
 EP.Arg_Parsed(name=:a, type=Any, default=ExprParsers.NoDefault())
 EP.Arg_Parsed(name=:(b::Int), type=:HasSomeTrait, default=ExprParsers.NoDefault())
```

That is nice, we captured `HasSomeTrait` in the type field. This is because `argument::StandardType::TraitsType` is evaluated as `(argument::StandardType)::TraitsType`. So we would like to get access to the `b::Int` as well.

But now things start to get difficult, because there is also `:a` with `type=Any`, and somehow we need to separate both cases.

We can easily do this by defining more specific parsers. Lets start with the "standard" Parser.
```jldoctest example
julia> parser_standard_arg = EP.Arg(name=EP.anysymbol)
EP.Arg(
  name    = ExprParsers.Isa{Symbol}()
  type    = ExprParsers.Isa{Any}()
  default = ExprParsers.Isa{Any}()
)
julia> parse_expr(parser_standard_arg, parsed_function.args[1])
EP.Arg_Parsed(
  name    = :a
  type    = Any
  default = ExprParsers.NoDefault()
)
julia> parse_expr(parser_standard_arg, parsed_function.args[2])
ERROR: ParseError: Expected type `Symbol`, got `b::Int` of type `Expr`.
```
That looks very good: Our new parser only parsers standard arguments and fails on traitor-like syntax. In addition it super simple - we just required the name to be a `Base.Symbol`.

Now let's define the traitor-argument. We know that the "name" field will be just another type-annotation. Let's use that knowledge straight away.
```jldoctest example
julia> parser_traitor_arg = EP.Arg(name=EP.TypeAnnotation())
EP.Arg(
  name    = EP.TypeAnnotation(name=ExprParsers.Isa{Any}(), type=ExprParsers.Isa{Any}())
  type    = ExprParsers.Isa{Any}()
  default = ExprParsers.Isa{Any}()
)
julia> parse_expr(parser_traitor_arg, parsed_function.args[1])
ERROR: ParseError: ExprParsers.TypeAnnotation has no `parse_expr` method defined to capture Type `Symbol`. Got: `a`.
julia> parse_expr(parser_traitor_arg, parsed_function.args[2])
EP.Arg_Parsed(
  name    = EP.TypeAnnotation_Parsed(name=:b, type=:Int)
  type    = :HasSomeTrait
  default = ExprParsers.NoDefault()
)
```
It worked. We only parsed traitor syntax, and now have all the components readily available.

We have all the pieces together, now we just need to combine them. `EP.AnyOf` is the natural choice to combine multiple parsers by falling back to the next if the first fails.
```jldoctest example
julia> parser_arg = EP.AnyOf(parser_standard_arg, parser_traitor_arg)
ExprParsers.AnyOf{Tuple{ExprParsers.Arg,ExprParsers.Arg}}((EP.Arg(name=ExprParsers.Isa{Symbol}(), type=ExprParsers.Isa{Any}(), default=ExprParsers.Isa{Any}()), EP.Arg(name=EP.TypeAnnotation(name=ExprParsers.Isa{Any}(), type=ExprParsers.Isa{Any}()), type=ExprParsers.Isa{Any}(), default=ExprParsers.Isa{Any}())), "")
julia> parse_expr(parser_arg, parsed_function.args[1])
EP.Arg_Parsed(
  name    = :a
  type    = Any
  default = ExprParsers.NoDefault()
)
julia> parse_expr(parser_arg, parsed_function.args[2])
EP.Arg_Parsed(
  name    = EP.TypeAnnotation_Parsed(name=:b, type=:Int)
  type    = :HasSomeTrait
  default = ExprParsers.NoDefault()
)
julia> parse_expr.(parser_arg, parsed_function.args)
2-element Array{ExprParsers.Arg_Parsed,1}:
 EP.Arg_Parsed(name=:a, type=Any, default=ExprParsers.NoDefault())
 EP.Arg_Parsed(name=EP.TypeAnnotation_Parsed(name=:b, type=:Int), type=:HasSomeTrait, default=ExprParsers.NoDefault())
```
There we are! We have build or parsing pipeline in a very straightforward and intuitive manner and ended up at having all the information parsed in a need way.


Extended Example: Part II Working with parsed results
-----------------------------------------------------

One thing which is missing though, is to extract the information in a convenient way. In julia the most convenient way is to use dispatch. But how to dispatch in the example above? Both versions are of type `ExprParsers.Arg_Parsed`.

We could have made the types more complicated, e.g. by including the types of all subparsers as well, however we decided against it and in favour for a simple meta-type `EP.Named{Tag}`. Bringing this into a meta-expr-parser has the big advantage that you can easily create your own ExprParsers without bothering about the precise types you use.

`EP.Named{Tag}(some_parser)` constructs a named version of your parser, which preserves the name when put through `parse_expr`. That is it's key purpose. Let's see it it in action.

```jldoctest example
julia> parser_arg_named = EP.AnyOf(
         EP.Named{:standard}(parser_standard_arg),
         EP.Named{:traitor}(parser_traitor_arg)
       )
ExprParsers.AnyOf{Tuple{ExprParsers.Named{:standard,ExprParsers.Arg},ExprParsers.Named{:traitor,ExprParsers.Arg}}}((ExprParsers.Named{:standard,ExprParsers.Arg}(EP.Arg(name=ExprParsers.Isa{Symbol}(), type=ExprParsers.Isa{Any}(), default=ExprParsers.Isa{Any}())), ExprParsers.Named{:traitor,ExprParsers.Arg}(EP.Arg(name=EP.TypeAnnotation(name=ExprParsers.Isa{Any}(), type=ExprParsers.Isa{Any}()), type=ExprParsers.Isa{Any}(), default=ExprParsers.Isa{Any}()))), "")
julia> parsed_args = parse_expr.(parser_arg_named, parsed_function.args)
2-element Array{ExprParsers.Named{Name,ExprParsers.Arg_Parsed} where Name,1}:
 ExprParsers.Named{:standard,ExprParsers.Arg_Parsed}(EP.Arg_Parsed(name=:a, type=Any, default=ExprParsers.NoDefault()))
 ExprParsers.Named{:traitor,ExprParsers.Arg_Parsed}(EP.Arg_Parsed(name=EP.TypeAnnotation_Parsed(name=:b, type=:Int), type=:HasSomeTrait, default=ExprParsers.NoDefault()))
```

We did the same as before, but added some type-tags to the results. You see that wrapping a parser into `EP.Named{Tag}(...)` will seamlessly and intuitively pass on the `Tag` to the parsed result. That is exactly what we need to simplify our dispatch.

Finally, let's write some utility which extracts all the Traitor information in a custom type.
```jldoctest example
julia> Base.@kwdef struct TraitorArg
         name
         type
         traitstype
         default
       end
TraitorArg
julia> extract_traitsarg(parsed::EP.Named{:standard}) = TraitorArg(
         name = parsed[].name,
         type = parsed[].type,
         traitstype = Any,
         default = parsed[].default,
       )
extract_traitsarg (generic function with 1 method)
julia> extract_traitsarg(parsed::EP.Named{:traitor}) = TraitorArg(
         name = parsed[].name.name,
         type = parsed[].name.type,
         traitstype = parsed[].type,
         default = parsed[].default,
       )
extract_traitsarg (generic function with 2 methods)
julia> traitor_args = extract_traitsarg.(parse_expr.(parser_arg_named, parsed_function.args))
2-element Array{TraitorArg,1}:
 TraitorArg(:a, Any, Any, ExprParsers.NoDefault())
 TraitorArg(:b, :Int, :HasSomeTrait, ExprParsers.NoDefault())
```

Super clean result and intuitive Julian style of programming. You can see that the use of `EP.Named` actually simplified the dispatch a lot and made it extremely readable.

From here on you can start to fill the traitor syntax with life. It turns out that we missed on one special Traitor-syntax case. Concretely, the argument style `a::::TraitsType` is officially supported, but not captured by either of the above. If I got you motivated, try to implement this case yourself!

I hope you enjoyed this extended example and got a good feeling of how to work with `ExprParsers`.


Extended Example: Part III creating Expr again
----------------------------------------------

Almost forgot: When defining your own macro there will come the time when you want to construct `Expr` objects again in order to return them to the user.

`ExprParsers` are also made for this task. One way to do this is to manipulate the parsed results directly. Let's see what this means.

```jldoctest
julia> using ExprParsers

julia> parsed = parse_expr(EP.Function(), :(f(x) = x))
EP.Function_Parsed(
  name    = :f
  curlies = Any[]
  args    = Any[:x]
  kwargs  = Any[]
  wheres  = Any[]
  body    = quote
    #= none:1 =#
    x
end
)
julia> parsed.name = :anothername;

julia> parsed
EP.Function_Parsed(
  name    = :anothername
  curlies = Any[]
  args    = Any[:x]
  kwargs  = Any[]
  wheres  = Any[]
  body    = quote
    #= none:1 =#
    x
end
)
julia> to_expr(parsed)
:(function anothername(x)
      #= none:1 =#
      x
  end)
```

The key is that every parsed result can be converted to an Expr by using `to_expr`.

The second way to construct Expr is by constructing parsed objects directly. You just have to use the `EP.Function_Parsed` instead of `EP.Function`, i.e. appending `_Parsed` to your parser type and you get the constructor for the parsed object.
It supports keyword assignment together with useful default values. In general it is very handy.

```jldoctest
julia> using ExprParsers

julia> func = EP.Function_Parsed(
         name = :myfunc,
         args = [:(a::T), EP.Arg_Parsed(name = :b, default = 42)],
         kwargs = [EP.Arg_Parsed(name = :c, default = :hi)],
         wheres = [:T],
         body = :(a + b)
       )
EP.Function_Parsed(
  name    = :myfunc
  curlies = Any[]
  args    = Any[:(a::T), EP.Arg_Parsed(name=:b, type=Any, default=42)]
  kwargs  = ExprParsers.Arg_Parsed[EP.Arg_Parsed(name=:c, type=Any, default=:hi)]
  wheres  = [:T]
  body    = :(a + b)
)
julia> to_expr(func)
:(function myfunc(a::T, b = 42; c = hi) where T
      a + b
  end)
```

For instance, for the Traitor syntax such manual construction would be needed, as quite some transformations need to happen to arrive at the traits semantics. As you can see, it is very easy to do this using `ExprParsers`.


Extended Example: Part IV Defining your own ExprParser with `EP.@exprparser`
----------------------------------------------------------------------------

As a final little excurse about the Traitor syntax example, I would like to show you how you can easily build
your own ExprParsers using the macro `EP.@exprparsers`. It is also used internally to most of the parsers available.

For example, let us rewrite the previous code which we used to extract Traitor information. For easier reference, here a self-containing copy of what we did above:
```jldoctest example4
julia> using ExprParsers

julia> parser_standard_arg = EP.Arg(name=EP.anysymbol);

julia> parser_traitor_arg = EP.Arg(name=EP.TypeAnnotation());

julia> parser_arg_named = EP.AnyOf(
         EP.Named{:standard}(parser_standard_arg),
         EP.Named{:traitor}(parser_traitor_arg),
       );

julia> Base.@kwdef struct TraitorArg
         name
         type
         traitstype
         default
       end
TraitorArg
julia> extract_traitsarg(parsed::EP.Named{:standard}) = TraitorArg(
         name = parsed[].name,
         type = parsed[].type,
         traitstype = Any,
         default = parsed[].default,
       )
extract_traitsarg (generic function with 1 method)
julia> extract_traitsarg(parsed::EP.Named{:traitor}) = TraitorArg(
         name = parsed[].name.name,
         type = parsed[].name.type,
         traitstype = parsed[].type,
         default = parsed[].default,
       )
extract_traitsarg (generic function with 2 methods)
julia> # which we then used for extraction

julia> parsed_function = parse_expr(EP.Function(), :(
         function f(a, b::Int::HasSomeTrait)
            "a = $a, b = $b, sometrait(b) = $(sometrait(b))"
         end
       ));

julia> traitor_args = extract_traitsarg.(parse_expr.(parser_arg_named, parsed_function.args))
2-element Array{TraitorArg,1}:
 TraitorArg(:a, Any, Any, ExprParsers.NoDefault())
 TraitorArg(:b, :Int, :HasSomeTrait, ExprParsers.NoDefault())
```

We can easily transform this into a full-fledged `ExprParser` by making using of the `EP.@exprparser` macro.
```jldoctest example4
julia> EP.@exprparser struct TraitorArgExprParser
         name = EP.anything
         type = EP.anything = Any
         traitstype = EP.anything = Any
         default = EP.anything = EP.nodefault
       end;

julia> function EP.parse_expr(parser::TraitorArgExprParser, expr)
         _parse_expr_traitor(parser, parse_expr(parser_arg_named, expr))
       end;

julia> _parse_expr_traitor(parser, parsed::EP.Named{:standard}) = parse_expr(parser,
         name = parsed[].name,
         type = parsed[].type,
         traitstype = Any,
         default = parsed[].default,
       )
_parse_expr_traitor (generic function with 1 method)
julia> _parse_expr_traitor(parser, parsed::EP.Named{:traitor}) = parse_expr(parser,
         name = parsed[].name.name,
         type = parsed[].name.type,
         traitstype = parsed[].type,
         default = parsed[].default,
       )
_parse_expr_traitor (generic function with 2 methods)
julia> function EP.to_expr(parsed::TraitorArgExprParser_Parsed)
         if parsed.default === EP.nodefault
           if parsed.traitstype == Any
             :($(parsed.name)::$(parsed.type))
           else
             :($(parsed.name)::$(parsed.type)::$(parsed.traitstype))
           end
         else
           # Note that function keyword arguments are constructed using `Expr(:kw, ...)` and not plain `=`
           if parsed.traitstype == Any   
             Expr(:kw, :($(parsed.name)::$(parsed.type)), parsed.default)
           else
             Expr(:kw, :($(parsed.name)::$(parsed.type)::$(parsed.traitstype)), parsed.default)
           end
         end
       end;

```
That defines the `ExprParser` as well as the key interface `parse_expr` and `to_expr`. Let's go through it one by one. First we defined the parser using `EP.@exprparser`. It is very much identical to defining a simple struct with `Base.kwdef` support, only that in addition you can make use of a neat double-default-syntax. `default = EP.anything = EP.nodefault` means that the "first" default value `EP.anything` is the default sub-parser which is used when constructing an `TraitorArgExprParser`, while the "second" default value `EP.nodefault` is the default value used when constructing the parsed result `TraitorArgExprParser_Parsed`.

Then we defined `parse_expr(...)` for our new type, which simply dispatches on the `Named{Tag}` parsers within the sub-function `_parse_expr_traitor` to actually construct the parsed results. One general feature we used here is that `parse_expr(...)` by default always supports a generic keyword syntax
`parse_expr(parser, field1 = :value_to_be_parsed, field2 = ...)` which translates to constructing the parsed result, with all fields matched by the respective sub-parsers
`..._Parsed(field1 = parse_expr(parser.field1, :value_to_be_parsed), field2 = ...)`. Using this syntax we make
sure that always all sub-parsers are actually used and not overlooked accidentally.

Finally we also overloaded `EP.to_expr` on top of the `..._Parsed` type so that we can easily convert traitor arguments back to proper `Base.Expr` objects.


It is not much to define, and everything very straightforward. Now we have a fully flexible full-fledged TraitorArgument parser. Let's see it in action.

```jldoctest example4
julia> parser_traitor_arg = TraitorArgExprParser(default = EP.Isa(Union{EP.NoDefault, Int}))
EP.TraitorArgExprParser(
  name       = ExprParsers.Isa{Any}()
  type       = ExprParsers.Isa{Any}()
  traitstype = ExprParsers.Isa{Any}()
  default    = ExprParsers.Isa{Union{ExprParsers.NoDefault, Int64}}()
)
julia> traitor_args = parse_expr.(parser_traitor_arg, parsed_function.args)
2-element Array{TraitorArgExprParser_Parsed,1}:
 EP.TraitorArgExprParser_Parsed(name=:a, type=Any, traitstype=Any, default=ExprParsers.NoDefault())
 EP.TraitorArgExprParser_Parsed(name=:b, type=:Int, traitstype=:HasSomeTrait, default=ExprParsers.NoDefault())
julia> traitor_arg = parse_expr(parser_traitor_arg, Expr(:kw, :(a::Int::TraitsType), 4))  # Note that we need to use `Expr(:kw, ...)` to construct function keyword arguments
EP.TraitorArgExprParser_Parsed(
  name       = :a
  type       = :Int
  traitstype = :TraitsType
  default    = 4
)
julia> to_expr(traitor_arg)
:($(Expr(:kw, :((a::Int)::TraitsType), 4)))
julia> parse_expr(parser_traitor_arg, Expr(:kw, :(a::String::TraitsType), "hi"))
ERROR: ParseError: Expected type `Union{ExprParsers.NoDefault, Int64}`, got `hi` of type `String`.
```
All works well, and also our example restriction to only accept `Int` default values, or no default at all, works as intended. As you see, the `ExprParsers` package even comes with pretty printing of your custom `@exprparser` type.

The `ExprParsers` package, with `parse_expr`, `to_expr` and `@exprparser` at its core, provides an interface which is easy to understand, easy to work with and easy to extend.
Having defined the interface, many tedious and repetitive `Expr`-parsing tasks are well encapsulated and it becomes much easier to construct further macro-semantics on top of it.

I hope you enjoy the package.
