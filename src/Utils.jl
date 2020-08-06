module Utils
export nonempty, unwrap_where, rewrap_where, split_where, issomething, change_name!,
  get_name, find_object, get_object,
  @ifsomething, iterate_start, IterateStart, Iterator,
  True, False,
  isexpr

using ProxyInterfaces

const True = Val{true}
const False = Val{false}

nonempty(a) = !isempty(a)

"""
    isexpr(expr) -> Bool
    isexpr(expr, head) -> Bool

Checks whether given value isa `Base.Expr` and if further given `head`, it also checks whether
the `head` matches `expr.head`.

# Examples
```jldoctest
julia> using ExprParsers

julia> EP.isexpr(:(a = hi))
true
julia> EP.isexpr(12)
false
julia> EP.isexpr(:(f(a) = a), :(=))
true
julia> EP.isexpr(:(f(a) = a), :function)
false
```
"""
isexpr(::Expr) = true
isexpr(other) = false
isexpr(expr::Expr, head::Symbol) = expr.head == head
isexpr(other, head::Symbol) = false

"""
needed to properly match function signatures

similar to Base.unwrap_unionall, but for Base.Expr
"""
function unwrap_where(e::Base.Expr)
  if e.head == :where
    unwrap_where(e.args[1])
  else
    e
  end
end

"""
similar to Base.rewrap_unionall, but for Base.Expr
"""
rewrap_where(unwrapped, original) = unwrapped
function rewrap_where(unwrapped, original::Base.Expr)
  if original.head == :where
    Base.Expr(:where, rewrap_where(unwrapped, original.args[1]), original.args[2:end]...)
  else
    unwrapped
  end
end

"""
Example
```
julia> ExprParsers.Utils.split_where(:(f(a::A) where {C, Y <: A} where B where A ))
(:(f(a::A)), Any[:A, :B, :C, :(Y <: A)])
```
"""
split_where(e) = (e, [])
function split_where(e::Base.Expr)
  if e.head == :where
    subexpr, wheres = split_where(e.args[1])
    # nested wheres to the back, they may depend on the outer where
    subexpr, [e.args[2:end]; wheres]
  else
    e, []
  end
end

Base.get(::Val{T}) where T = T

issomething(::Nothing) = false
issomething(::Any) = true

"""
    IterTools.@ifsomething expr
If `expr` evaluates to `nothing`, equivalent to `return nothing`, otherwise the macro
evaluates to the value of `expr`. Not exported, useful for implementing iterators.
```jldoctest
julia> using ExprParsers

julia> ExprParsers.@ifsomething iterate(1:2)
(1, 1)
julia> let elt, state = ExprParsers.@ifsomething iterate(1:2, 2); println("not reached"); end
```
"""
macro ifsomething(ex)
    quote
        result = $(esc(ex))
        result === nothing && return nothing
        result
    end
end

# little helper to write easier code for iteration (CAUTION: this will lead to type-instable code)
struct IterateStart end
const iterate_start = IterateStart()
Base.iterate(a, ::IterateStart) = Base.iterate(a)


# possibility to use Iterators as elementwise Parsers of possibly unknown length
"""
    EP.Iterator(some_iterable)

Mark an iterable explicitly as an Iterator to add support for elementwise `parse_expr`.

# Examples

```jldoctest
julia> using ExprParsers; using Base.Iterators

julia> parser = EP.Iterator(repeated(4));

julia> parse_expr(parser, [4, 4])
2-element Array{Int64,1}:
 4
 4
julia> parse_expr(parser, [4, 4, 4, 4])
4-element Array{Int64,1}:
 4
 4
 4
 4
julia> parse_expr(parser, [3, 4])
ERROR: ParseError: Using default `==` comparison, but parser `4` ≠ value `3`.
```
"""
struct Iterator{T}
  iterator::T
end
ProxyInterfaces.iterator(i::Iterator) = i.iterator
ProxyInterfaces.iterator(::Base.Type{Iterator{T}}) where T = T
ProxyInterfaces.@iterator Iterator

end  # module
