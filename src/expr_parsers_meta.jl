# Generic Parser Helper
# =====================

"""
  identify Parser by Tag

Named Parsers can be easily identified by Type. The Nametag is also passed to the parsed value.
"""
@def_structequal struct Named{Name, T} <: ExprParser
  expr::T
  Named{Name}(parser::T) where {Name, T} = new{Name, T}(parser)
end
@def_structequal struct Named_Parsed{Name, T} <: ExprParsed
  expr
end
ExprParsed(::Base.Type{Named}) = Named_Parsed
ExprParsed(::Base.Type{Named{Name}}) where Name = Named_Parsed{Name}
ExprParsed(::Base.Type{Named{Name, T}}) where {Name, T} = Named_Parsed{Name, T}

function parse_expr(parser::Named{Name, T}, expr) where {Name, T}
  parsed = parse_expr(parser.expr, expr)
  Named_Parsed{Name, T}(parsed)
end
to_expr(parsed::Named_Parsed) = to_expr(parsed.expr)


"""
adds a Mapping Layer to a possibly nested Parser with which you can refer into a deep nested subparser by name
"""
@def_structequal struct Indexed{T} <: ExprParser
  _mapping::Dict
  _paths::Dict
  expr::T
  function Indexed(func_dict!)
    mapping = Dict()
    expr = func_dict!(mapping)
    paths = Dict(k => find_object(v, expr) for (k,v) in mapping)
    new{typeof(expr)}(mapping, paths, expr)
  end
end

@def_structequal struct Indexed_Parsed{T} <: ExprParsed
  _mapping::Dict
  _paths::Dict
  expr
end
ProxyInterface.dict(parser::Indexed) = parser._mapping
ProxyInterface.dict(::Base.Type{Indexed{T}}) where T = T
ProxyInterface.@dict Indexed

ProxyInterface.dict(parsed::Indexed_Parsed) = parsed._mapping
ProxyInterface.dict(::Base.Type{Indexed_Parsed{T}}) where T = T
ProxyInterface.@dict Indexed_Parsed

ExprParsed(::Base.Type{Indexed}) = Indexed_Parsed
ExprParsed(::Base.Type{Indexed{T}}) where T = Indexed_Parsed{T}

function parse_expr(parser::Indexed{T}, expr) where T
  parsed = parse_expr(parser.expr, expr)
  mapping = Dict(k => get_object(parsed, nested_index) for (k, nested_index) in parser._paths)
  Indexed_Parsed{T}(mapping, parser._paths, parsed)
end
to_expr(parsed::Indexed_Parsed) = to_expr(parsed.expr)

function Base.setindex!(parsed::Indexed_Parsed, value, key)
  nested_index = parsed._paths[key]
  # we need to update both the ._mapping as well as the .expr
  parsed._mapping[key] = value
  set_object!(parsed.expr, value, nested_index)
end


# Helper
# ------

# TODO these little helpers might be well replaced with some Lense Library
# but for now they are enough

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

function get_object(nested, nested_index, get_key)
  if length(nested_index) == 0
     # return everything for empty index
    nested
  else
    subobject = get_key(nested, nested_index[1])

    if length(nested_index) == 1
      subobject
    else
      get_object(subobject, nested_index[2:end])
    end
  end
end
get_object(nested::Union{Vector, Tuple, Dict}, nested_index) = get_object(nested, nested_index, getindex)
get_object(nested, nested_index) = get_object(nested, nested_index, getproperty)


function set_object!(nested, value, nested_index, get_key, set_key!)
  @assert length(nested_index) > 0 "we should never reach empty index in this recursion"
  if length(nested_index) == 1
    # use the last index to update the object
    set_key!(nested, value, nested_index[1])
  else
    subobject = get_key(nested, nested_index[1])
    newsubobject = set_object!(subobject, value, nested_index[2:end])
    set_key!(nested, newsubobject, nested_index[1])
  end
  nested
end
function set_object!(nested::Union{Vector, Tuple, Dict}, value, nested_index)
  set_object!(nested, value, nested_index, getindex, setindex!)
  nested
end
function set_object!(nested, value, nested_index)
  set_object!(nested, value, nested_index, getproperty, (object, value, key) -> setproperty!(object, key, value))
  nested
end
