module Utils
export nonempty, unwrap_where, rewrap_where, split_where, issomething, change_name!,
  get_name, find_object, get_object,
  @ifsomething, iterate_start, IterateStart,
  True, False

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
  return list of keys/symbols to access a reference or nothing if no is found
"""
function find_object(obj, nested::Union{Vector, Tuple, Dict})
  # first check whether the object is any of the elements itself
  i = findfirst(nested) do x
    obj === x
  end
  issomething(i) && return i

  # recurse if nothing found
  # return first non-nothing value
  for (k, v) in zip(keys(nested), values(nested))
    subkeys = find_object(obj, v)
    issomething(subkeys) && return [k; subkeys]
  end
  return nothing
end
find_object(obj, nested) = find_object(obj, Dict(key => getproperty(nested, key) for key in propertynames(nested)))

function get_object(nested_index, nested, get_key)
  if length(nested_index) == 0
     # return everything for empty index
    nested
  else
    subobject = get_key(nested, nested_index[1])

    if length(nested_index) == 1
      subobject
    else
      get_object(nested_index[2:end], subobject)
    end
  end
end
get_object(nested_index, nested::Union{Vector, Tuple, Dict}) = get_object(nested_index, nested, getindex)
get_object(nested_index, nested) = get_object(nested_index, nested, getproperty)


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

end  # module
