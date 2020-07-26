var documenterSearchIndex = {"docs":
[{"location":"library/","page":"Library","title":"Library","text":"CurrentModule = ExprParsers","category":"page"},{"location":"library/#Public-API","page":"Library","title":"Public API","text":"","category":"section"},{"location":"library/","page":"Library","title":"Library","text":"TBD","category":"page"},{"location":"library/","page":"Library","title":"Library","text":"","category":"page"},{"location":"library/","page":"Library","title":"Library","text":"Modules = [ExprParsers]","category":"page"},{"location":"library/#ExprParsers.ExprParsers","page":"Library","title":"ExprParsers.ExprParsers","text":"ExprParsers\n\nthe main interface encompass just three concepts, which seamlessly interact with oneanother\n\nmacro @exprparser: easily create definitions for highly flexible and nestable parsers\nfunction parse_expr: compares a matcher with a value, and returns a parsed value\nfunction to_expr: transforms parsed values back to AbstractSyntaxTrees\n\n\n\n\n\n","category":"module"},{"location":"library/#ExprParsers.Arg","page":"Library","title":"ExprParsers.Arg","text":"parses:\n\na\na::B\n::B\na = c\na::B = c\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Arg_Parsed","page":"Library","title":"ExprParsers.Arg_Parsed","text":"parses:\n\na\na::B\n::B\na = c\na::B = c\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Assignment","page":"Library","title":"ExprParsers.Assignment","text":"parses:\n\nleft = right\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Assignment_Parsed","page":"Library","title":"ExprParsers.Assignment_Parsed","text":"parses:\n\nleft = right\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Block","page":"Library","title":"ExprParsers.Block","text":"parses standard blocks of code or Vectors of Base.Expr\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Call","page":"Library","title":"ExprParsers.Call","text":"parses:\n\na()\na(b, c)\na{b, c}(d, e)\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Call_Parsed","page":"Library","title":"ExprParsers.Call_Parsed","text":"parses:\n\na()\na(b, c)\na{b, c}(d, e)\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Expr","page":"Library","title":"ExprParsers.Expr","text":"parses:\n\nBase.Expr(head, args...)\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.ExprParsed","page":"Library","title":"ExprParsers.ExprParsed","text":"ExprParsed(parser::ExprParserWithParsed)::ExprParsed\n\nMaps Parser to respective Parsed type, and is also abstract super type of all Parsed types.\n\nExample\n\nEP.ExprParsed(EP.Assignment) == EP.Assignment_Parsed\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.ExprParser","page":"Library","title":"ExprParsers.ExprParser","text":"All parsers in the ExprParsers package inherit from this type.\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.ExprParserWithParsed","page":"Library","title":"ExprParsers.ExprParserWithParsed","text":"This is mainly for internal usage. Please use @exprparser instead for the public interface.\n\nSubtype of ExprParser which indicates that this parser actually constructs a ExprParsed object when calling parse_expr. The resulting ExprParsed object is a struct with identical fields like the parser, where then the parsed values will be stored.\n\nExprParsed(parser::ExprParserWithParsed) will return the corresponding ExprParsed type.\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Expr_Parsed","page":"Library","title":"ExprParsers.Expr_Parsed","text":"parses:\n\nBase.Expr(head, args...)\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Function","page":"Library","title":"ExprParsers.Function","text":"parses:\n\nfunction a(b, c) where B\n  d\nend\na(b, c) = d\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Function_Parsed","page":"Library","title":"ExprParsers.Function_Parsed","text":"parses:\n\nfunction a(b, c) where B\n  d\nend\na(b, c) = d\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Indexed","page":"Library","title":"ExprParsers.Indexed","text":"adds a Mapping Layer to a possibly nested Parser with which you can refer into a deep nested subparser by name\n\nuse like\n\nEP.Indexed() do dict\n  EP.Expr(quote\n    a = $(dict[:a] = EP.Isa(Int))\n    b = $(dict[:b] = EP.anysymbol)\n  end)\nend\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Macro","page":"Library","title":"ExprParsers.Macro","text":"parses:\n\n@macroname arg1 arg2 ...\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Macro_Parsed","page":"Library","title":"ExprParsers.Macro_Parsed","text":"parses:\n\n@macroname arg1 arg2 ...\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Named","page":"Library","title":"ExprParsers.Named","text":"identify Parser by Tag\n\nNamed Parsers can be easily identified by Type. The Nametag is also passed to the parsed value.\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.NestedDot","page":"Library","title":"ExprParsers.NestedDot","text":"parses:\n\na.b\nfun(T{:hi}).b.c.d.e.f\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.NestedDot_Parsed","page":"Library","title":"ExprParsers.NestedDot_Parsed","text":"parses:\n\na.b\nfun(T{:hi}).b.c.d.e.f\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Reference","page":"Library","title":"ExprParsers.Reference","text":"parses:\n\na\na{b, c}\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Reference_Parsed","page":"Library","title":"ExprParsers.Reference_Parsed","text":"parses:\n\na\na{b, c}\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Signature","page":"Library","title":"ExprParsers.Signature","text":"parses:\n\na(b, c::Any)\na(b::B, c) where B\n(::Any, c::C) where {C <: Number}\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Signature_Parsed","page":"Library","title":"ExprParsers.Signature_Parsed","text":"parses:\n\na(b, c::Any)\na(b::B, c) where B\n(::Any, c::C) where {C <: Number}\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Type","page":"Library","title":"ExprParsers.Type","text":"parses:\n\na\na{b, c}\na{d} where d\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.TypeAnnotation","page":"Library","title":"ExprParsers.TypeAnnotation","text":"parses:\n\na\na::B\n::B\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.TypeAnnotation_Parsed","page":"Library","title":"ExprParsers.TypeAnnotation_Parsed","text":"parses:\n\na\na::B\n::B\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.TypeRange","page":"Library","title":"ExprParsers.TypeRange","text":"parses:\n\nTypeVar >: LowerBound\nTypeVar <: UpperBound\nLowerBound <: TypeVar <: UpperBound\n\nNote: Construct with typevar = anysymbol to guarantee that only plain symbols can be used as type variable\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.TypeRange_Parsed","page":"Library","title":"ExprParsers.TypeRange_Parsed","text":"parses:\n\nTypeVar >: LowerBound\nTypeVar <: UpperBound\nLowerBound <: TypeVar <: UpperBound\n\nNote: Construct with typevar = anysymbol to guarantee that only plain symbols can be used as type variable\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers.Type_Parsed","page":"Library","title":"ExprParsers.Type_Parsed","text":"parses:\n\na\na{b, c}\na{d} where d\n\n\n\n\n\n","category":"type"},{"location":"library/#ExprParsers._find_object-Tuple{Any,Union{Tuple, Dict, Array{T,1} where T}}","page":"Library","title":"ExprParsers._find_object","text":"return list of keys/symbols to access a reference or nothing if no is found\n\n\n\n\n\n","category":"method"},{"location":"library/#ExprParsers._get_struct_name-Tuple{Expr}","page":"Library","title":"ExprParsers._get_struct_name","text":"returns name of struct definition as symbol\n\n\n\n\n\n","category":"method"},{"location":"library/#ExprParsers._parsed_struct_from_parser_struct-Tuple{Expr,Any}","page":"Library","title":"ExprParsers._parsed_struct_from_parser_struct","text":"makes it mutable\nadds SUFFIX to struct name\ndeletes all default values\n\n\n\n\n\n","category":"method"},{"location":"library/#ExprParsers.parse_expr-Tuple{Any,Any}","page":"Library","title":"ExprParsers.parse_expr","text":"parse_expr(parser, value)\n\nMatch parser against a value, will throw ParseError if the parser does not match.\n\nDefaults to comparing with ==, if matches, will return the value. Parsers will be called instead and return their parsed value.\n\n\n\n\n\n","category":"method"},{"location":"library/#ExprParsers.parse_expr-Tuple{ExprParsers.ExprParserWithParsed}","page":"Library","title":"ExprParsers.parse_expr","text":"All ExprParserWithParsed have a common parse_expr method, namely that all struct fields are given directly as keyword arguments.\n\n\n\n\n\n","category":"method"},{"location":"library/#ExprParsers.to_expr-Tuple{Any}","page":"Library","title":"ExprParsers.to_expr","text":"to_expr(parsed)\n\nConverts parsed information back to Expr.\n\nDefaults to returning same value, however if something knows about how it can be translated back, just overload the function.\n\n\n\n\n\n","category":"method"},{"location":"library/#ExprParsers.@passert-Tuple{Any,Vararg{Any,N} where N}","page":"Library","title":"ExprParsers.@passert","text":"@passert cond [text]\n\nThrow an ParseError if cond is false. Preferred syntax for writing assertions. Message text is optionally displayed upon assertion failure.\n\nExamples\n\njulia> @passert iseven(3) \"3 is an odd number!\"\nERROR: ParseError: 3 is an odd number!\njulia> @passert isodd(3) \"What even are numbers?\"\n\nAdapted from Base.@assert\n\n\n\n\n\n","category":"macro"},{"location":"manual/#Manual","page":"Manual","title":"Manual","text":"","category":"section"},{"location":"manual/","page":"Manual","title":"Manual","text":"The main interface encompass just three concepts, which seamlessly interact with oneanother","category":"page"},{"location":"manual/","page":"Manual","title":"Manual","text":"macro @exprparser: easily create definitions for highly flexible and nestable parsers\nfunction parse_expr: compares a matcher with a value, and returns a parsed value\nfunction to_expr: transforms parsed values back to AbstractSyntaxTrees","category":"page"},{"location":"manual/","page":"Manual","title":"Manual","text":"TBD","category":"page"},{"location":"manual/#Installation","page":"Manual","title":"Installation","text":"","category":"section"},{"location":"manual/","page":"Manual","title":"Manual","text":"The package is soon going to be registered at General, until then you can use it by adding a custom registry.","category":"page"},{"location":"manual/","page":"Manual","title":"Manual","text":"using Pkg\npkg\"registry add https://github.com/JuliaRegistries/General\"  # central julia registry\npkg\"registry add https://github.com/schlichtanders/SchlichtandersJuliaRegistry.jl\"  # custom registry\npkg\"add ExprParsers\"","category":"page"},{"location":"manual/","page":"Manual","title":"Manual","text":"Use it like","category":"page"},{"location":"manual/","page":"Manual","title":"Manual","text":"using ExprParsers","category":"page"},{"location":"#ExprParsers.jl","page":"Home","title":"ExprParsers.jl","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"ExprParsers is a library made to simplify development of elaborate macros.","category":"page"},{"location":"","page":"Home","title":"Home","text":"What ExprParsers offers is a set of curated parsers for common Expr patterns. For example parse_expr(ExprParsers.Function(), :(f(a) = 2a)) will give you a ExprParsers.Function_Parsed object where you can inspect and change name, args, kwargs, curlies, wheres, and the function body. It just works and you don't have to bother any longer that you can also write the same function as function f(a); 2a; end - the parser handles this for you.","category":"page"},{"location":"","page":"Home","title":"Home","text":"In macros you often not only want to inspect the given Expr in efficient and stable manners, but also may want to change parts and return a respectively adapted Expr. For this purpose, all Parsed objects can be converted back to Expr by using the to_expr() method.","category":"page"},{"location":"","page":"Home","title":"Home","text":"We guarantee that parse_expr and to_expr are working nicely together, i.e. the following always holds for arbitrary expressions and parsers","category":"page"},{"location":"","page":"Home","title":"Home","text":"using ExprParsers  # comes with a shorthand EP for ExprParsers\nparser = EP.Function()\nexpr = :(f(a) = 2a))\nparsed = parse_expr(parser, expr)\n\n# applying the parser \"twice\" returns always the same parsed result\nparse_expr(parser, to_expr(parsed)) == parsed","category":"page"},{"location":"","page":"Home","title":"Home","text":"Note that ExprParsers exports a constant EP which is an alias for the package ExprParsers itself. This comes in very handy when you use the custom parsers a lot.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Checkout the test/ directory for seeing more examples, especially test/expr_parsers_with_parsed.jl where for each common Expr pattern a parser is put into action.","category":"page"},{"location":"#Manual-Outline","page":"Home","title":"Manual Outline","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Pages = [\"manual.md\"]","category":"page"},{"location":"#main-index","page":"Home","title":"Library Index","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Pages = [\"library.md\"]","category":"page"}]
}
