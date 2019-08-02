"""
needed to properly match function signatures

similar to Base.unwrap_unionall, but for Expr
"""
function unwrap_where(e::Expr)
  if e.head == :where
    unwrap_where(e.args[1])
  else
    e
  end
end

"""
similar to Base.rewrap_unionall, but for Expr
"""
rewrap_where(unwrapped, original) = unwrapped
function rewrap_where(unwrapped, original::Expr)
  if original.head == :where
    Expr(:where, rewrap_where(unwrapped, original.args[1]), original.args[2:end]...)
  else
    unwrapped
  end
end

"""
Example
```
julia> Continuables.split_where(:(f(a::A) where A where B where {C, Y<: A}))
(:(f(a::A)), Any[:A, :B, :C, :(Y <: A)])
```
"""
split_where(e) = (e, [])
function split_where(e::Expr)
  if e.head == :where
    subexpr, wheres = split_where(e.args[1])
    subexpr, [wheres; e.args[2:end]]  # nested wheres to the front
  else
    e, []
  end
end


"""
works with references and structs so far
"""
function change_name!(func_change_old_name, expr::Expr)
  if expr.head == :block
    change_name!(func_change_old_name, expr.args[1])

  elseif expr.head == :struct
    # first boolean arg represents whether the struct is mutable or not
    if expr.args[2] isa Symbol
      expr.args[2] = func_change_old_name(expr.args[2])
      return
    else
      change_name!(func_change_old_name, expr.args[2])
    end

  elseif expr.head == :(<:)
    if expr.args[1] isa Symbol
      expr.args[1] = func_change_old_name(expr.args[1])
    else
      change_name!(func_change_old_name, expr.args[1])
    end
  
  elseif expr.head == :(::)
    if expr.args[1] isa Symbol
      expr.args[1] = func_change_old_name(expr.args[1])
    else
      change_name!(func_change_old_name, expr.args[1])
    end

  else
    error("not yet supported to change name: $(expr)")
  end
end

function get_name(expr::Expr)
  name = Ref{Symbol}
  change_name!(expr) do old_name
    name.x = old_name
    old_name
  end
  name.x
end
