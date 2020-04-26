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

function parse_expr(np::Named{Name, T}, expr) where {Name, T}
  parsed = parse_expr(np.expr, expr)
  Named_Parsed{Name, T}(parsed)
end
to_expr(npp::Named_Parsed) = to_expr(npp.expr)


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

function parse_expr(ip::Indexed{T}, expr) where T
  parsed = parse_expr(ip.expr, expr)
  mapping = Dict(k => get_object(nested_index, parsed) for (k, nested_index) in ip._paths)
  Indexed_Parsed{T}(mapping, ip._paths, parsed)
end
to_expr(ipp::Indexed_Parsed) = to_expr(ipp.expr)





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
