"""
    ExprParsers

the main interface encompass just three concepts, which seamlessly interact with oneanother
- macro @parserfactory: easily create definitions for highly flexible and nestable parsers
- function ExprParsers.match: compares a matcher with a value, and returns a parsed value
- function toAST: transforms parsed values back to AbstractSyntaxTrees
"""
module ExprParsers
export @passert, ParseError,
  Matchers, Matcher, match, anything,
  Parsers, toAST, @parserfactory, Parser, Parsed,
  Named, Named_Parsed, Indexed, Indexed_Parsed,
  nodefault, NoDefault

include("exceptions.jl")

include("Matchers.jl")
using .Matchers
# we need to assign a const to match in order to refer to Matchers.match and not Base.match
const match = Matchers.match

include("Parsers/Parsers.jl")
using .Parsers
end # module
