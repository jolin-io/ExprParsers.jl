module Utils
export nonempty, unwrap_where, rewrap_where, split_where, issomething, change_name!,
  get_name, find_object, get_object,
  @ifsomething, iterate_start, IterateStart, Iterator,
  True, False

using ProxyInterface

const True = Val{true}
const False = Val{false}

nonempty(a) = !isempty(a)

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
julia> ASTParser.split_where(:(f(a::A) where {C, Y <: A} where B where A ))
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
julia> IterTools.@ifsomething iterate(1:2)
(1, 1)
julia> let elt, state = IterTools.@ifsomething iterate(1:2, 2); println("not reached"); end
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
struct Iterator{T}
  iterator::T
end
ProxyInterface.iterator(i::Iterator) = i.iterator
ProxyInterface.iterator(::Base.Type{Iterator{T}}) where T = T
ProxyInterface.@iterator Iterator

end  # module
