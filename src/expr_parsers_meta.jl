# Generic Parser Helper
# =====================

"""
  identify Parser by Tag

Named Parsers can be easily identified by Type. The Nametag is also passed to the parsed value.
"""
@def_structequal struct Named{Name, T} <: ExprParser
  value::T
  Named{Name}(value::T) where {Name, T} = new{Name, T}(value)
end

function parse_expr(parser::Named{Name}, expr) where Name
  parsed = parse_expr(parser.value, expr)
  Named{Name}(parsed)
end
to_expr(parsed::Named) = to_expr(parsed.value)


"""
adds a Mapping Layer to a possibly nested Parser with which you can refer into a deep nested subparser by name

use like
```
EP.Indexed() do dict
  EP.Expr(quote
    a = \$(dict[:a] = EP.Isa(Int))
    b = \$(dict[:b] = EP.anysymbol)
  end)
end
```
"""
@def_structequal struct Indexed{T} <: ExprParser
  _mapping::Dict
  _paths::Dict
  value::T
end
function Indexed(func_dict!)
  mapping = Dict()
  value = func_dict!(mapping)
  paths = Dict(k => _find_object(v, value) for (k,v) in mapping)
  Indexed(mapping, paths, value)
end

ProxyInterface.dict(parser::Indexed) = parser._mapping
ProxyInterface.dict(::Base.Type{Indexed{T}}) where T = T
ProxyInterface.@dict Indexed

function parse_expr(parser::Indexed{T}, expr) where T
  parsed = parse_expr(parser.value, expr)
  mapping = Dict(k => _get_object(parsed, nested_index) for (k, nested_index) in parser._paths)
  Indexed(mapping, parser._paths, parsed)
end
to_expr(parsed::Indexed) = to_expr(parsed.value)

function Base.setindex!(parsed::Indexed, value, key)
  nested_index = parsed._paths[key]
  # we need to update both the ._mapping as well as the .expr
  parsed._mapping[key] = value
  _set_object!(parsed.value, value, nested_index)
end


# Helper
# ------

# TODO these little helpers might be well replaced with some Lense Library
# but for now they are enough

"""
  return list of keys/symbols to access a reference or nothing if no is found
"""
function _find_object(obj, nested::Union{Vector, Tuple, Dict})
  # first check whether the object is any of the elements itself
  i = findfirst(nested) do x
    obj === x
  end
  issomething(i) && return i

  # recurse if nothing found
  # return first non-nothing value
  for (k, v) in zip(keys(nested), values(nested))
    subkeys = _find_object(obj, v)
    issomething(subkeys) && return [k; subkeys]
  end
  return nothing
end
_find_object(obj, nested) = _find_object(obj, Dict(key => getproperty(nested, key) for key in propertynames(nested)))

function _get_object(nested, nested_index, get_key)
  if length(nested_index) == 0
     # return everything for empty index
    nested
  else
    subobject = get_key(nested, nested_index[1])

    if length(nested_index) == 1
      subobject
    else
      _get_object(subobject, nested_index[2:end])
    end
  end
end
_get_object(nested::Union{Vector, Tuple, Dict}, nested_index) = _get_object(nested, nested_index, getindex)
_get_object(nested, nested_index) = _get_object(nested, nested_index, getproperty)


function _set_object!(nested, value, nested_index, get_key, set_key!)
  @assert length(nested_index) > 0 "we should never reach empty index in this recursion"
  if length(nested_index) == 1
    # use the last index to update the final object
    set_key!(nested, value, nested_index[1])
  else
    # otherwise recurse into subobject
    subobject = get_key(nested, nested_index[1])
    _set_object!(subobject, value, nested_index[2:end])
  end
  nested
end
function _set_object!(nested::Union{Vector, Tuple, Dict}, value, nested_index)
  _set_object!(nested, value, nested_index, getindex, setindex!)
  nested
end
function _set_object!(nested, value, nested_index)
  _set_object!(nested, value, nested_index, getproperty, (object, value, key) -> setproperty!(object, key, value))
  nested
end
