module ASTParser
export @parserfactory, toAST, 
  SymbolParser, ExprParser, ReferenceExprParser, TypeExprParser, CallExprParser

# interface
# =========

#=
the main interface encompass just three concepts, which seamlessly interact with oneanother
- @parserfactory (internally Parser): create highly flexible and nestable parsers
- function match: compares a matcher with a value, also defining a parsed return value
- function toAST: transforms parsed values back to AbstractSyntaxTrees
=#

"""
  This is only for internal use. Please see @parserfactory for the public interface

Types which constructor creates a Parser that can parses Expr
into a struct with identical fields, where then the parsed values will be SingletonParser_ValuesParsed
"""
abstract type Parser end 


"""
  match something against a value

will throw AssertError if does not match

if matches, by default will return the value if matches
however Parsers will be called instead and return their parsed value
"""
match(parser::Parser, value) = parser(value)
function match(matcher, value)
  @assert matcher == value
  value
end

"""
  convert called information back to AbstractSyntaxTree

defaults to returning same value
however if something knows about how it can be translated, just overload the function
"""
toAST(a) = a


include("utils.jl")
include("matchers.jl")
include("syntax.jl")
include("parser.jl")

end # module