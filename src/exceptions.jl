
"""
    ParseError

Special Exception Type to indicate that some parsing failed.
"""
struct ParseError <: Exception
  msg::AbstractString
end
ParseError() = ParseError("")
Base.showerror(io::IO, e::ParseError) = print(io, "ParseError: ", e.msg)


# for better error printing:
subexprs(any) = []
subexprs(symbol::Base.Symbol) = [symbol]
subexprs(expr::Base.Expr) = subexprs(expr, Val(expr.head))
subexprs(expr, ::Val{:block}) = [(subexprs(subexpr) for subexpr in expr.args if !isa(subexpr, LineNumberNode))... ;]
subexprs(expr, ::Val{:call}) = [expr; (subexprs(subexpr) for subexpr in expr.args[2:end])...]  # skip the function name
subexprs(expr, ::Val{:.}) = [expr; subexprs(expr.args[1])]
subexprs(expr, ::Val{:ref}) = [expr; subexprs(expr.args[1])]  # x[1]
subexprs(expr, ::Union{Val{:tuple}, Val{:braces}, Val{:bracescat}, Val{:vect}, Val{:vcat}, Val{:hcat}}) = [expr; (subexprs(subexpr) for subexpr in expr.args)...]
subexprs(expr, ::Val{:macrocall}) = [expr; (subexprs(subexpr) for subexpr in expr.args[2:end] if !isa(subexpr, LineNumberNode))...]
subexprs(expr, ::Val{:comparison}) = [expr; (subexprs(subexpr) for subexpr in expr.args[1:2:end])...]  # only odd entries
subexprs(expr, ::Val{:where}) = [expr]  # don't step into where because of TypeVariables
subexprs(expr, ::Union{Val{:<:}, Val{:>:}}) = [expr; subexprs(expr.args[1]); subexprs(expr.args[2])]
subexprs(expr, ::Val{:(=)}) = [expr; subexprs(expr.args[2])]


"""
    @passert cond [text]

Throw an [`ParseError`](@ref) if `cond` is `false`. Preferred syntax for writing assertions.
Message `text` is optionally displayed upon assertion failure.

If no text is given a default
rich text description is constructed, evaluating all found subexpressions for easier debugging.

# Examples
```jldoctest
julia> using ExprParsers

julia> @passert iseven(3) "3 is an odd number!"
ERROR: ParseError: 3 is an odd number!
julia> @passert isodd(3) "What even are numbers?"

julia> a = 3;

julia> @passert a+2 == 4
ERROR: ParseError: a + 2 == 4 = false
  a + 2 = 5
  a = 3
```

Adapted from Base.@assert
"""
macro passert(ex, msgs...)
  msg = if isempty(msgs)
    # As default message we print the expression and all subexpressions of interest together with their evaluation in the outer context
    all_strings = [:($(string(e)) * " = " * repr($(esc(e)))) for e in subexprs(ex)]
    reduce(all_strings) do x, y
      :($x * " \n  " * $y)
    end
    # TODO performance could be improved by reusing subresults
  else
    esc(msgs[1])
  end
  return :($(esc(ex)) ? $(nothing) : throw(ParseError($msg)))
end
