# Generic Parser Helper
# =====================

"""
    EP.Named{:MyTag}(parser)

Construct an ExprParser with a type identified by `MyTag`. This is helpful if you
have multiple versions of a similar parser and would like to easily distinguish them during dispatch.

The `Named{:MyTag}` wrapper is also passed on to the parsed value.

Works with any ExprParser, also custom defined ones.

# Examples
```jldoctest
julia> using ExprParsers

julia> parser1 = EP.Named{:simple}(EP.Assignment(left = EP.anysymbol));

julia> parser2 = EP.Named{:any}(EP.Assignment());

julia> parse_expr(parser1, :(a = 4))
ExprParsers.Named{:simple,ExprParsers.Assignment_Parsed}(EP.Assignment_Parsed(left=:a, right=4))
julia> parse_expr(parser1, :(a.b = 4))
ERROR: ParseError: Expected type `Symbol`, got `a.b` of type `Expr`.
julia> parse_expr(parser2, :(a = 4))
ExprParsers.Named{:any,ExprParsers.Assignment_Parsed}(EP.Assignment_Parsed(left=:a, right=4))
julia> parse_expr(parser2, :(a.b = 4))
ExprParsers.Named{:any,ExprParsers.Assignment_Parsed}(EP.Assignment_Parsed(left=:(a.b), right=4))
```
"""
@struct_hash_equal struct Named{Name, T} <: ExprParser
  value::T
  Named{Name}(value::T) where {Name, T} = new{Name, T}(value)
end

function parse_expr(parser::Named{Name}, expr) where Name
  parsed = parse_expr(parser.value, expr)
  Named{Name}(parsed)
end
to_expr(parsed::Named) = to_expr(parsed.value)

Base.getindex(named::Named) = named.value


@doc raw"""
    EP.Indexed(func_expecting_dict_as_only_argument)

Constructs an ExprParser where you can access dedicated subexpressions/subparsers via Dictionary lookup.
Most importantly, the shortcuts are preserved during `parse_expr()`.

Works with any ExprParser, also custom defined ones.

Concretely, here a toy example
```julia
EP.Indexed() do dict
  EP.Expr(quote
    a = $(dict[:a] = EP.Isa(Int))
    b = $(dict[:b] = EP.anysymbol)
  end)
end
```
As you can see, `EP.Indexed` is expecting a function which takes a `dict` as the only argument.
It best used with do-notation. The function then needs to return an `ExprParser`, but can do whatever it wants
in principle.
Shortcuts are now assigned by just using interpolation syntax `$(...)` and storing references to subparser
into the given `dict`. For example you see that `EP.Isa(Int)` is captured as `dict[:a]` before being used as a
subparser.

# Examples
```jldoctest
julia> using ExprParsers

julia> parser = EP.Indexed() do dict
         EP.Expr(quote
           a = $(dict[:a] = EP.Isa(Int))
           b = $(dict[:b] = EP.anysymbol)
         end)
       end;

julia> parser[:a]
ExprParsers.Isa{Int64}()
julia> parsed = parse_expr(parser, quote
         a = 42
         b = a
       end);

julia> parsed[:a], parsed[:b]
(42, :a)

julia> parse_expr(parser, quote
         a = 42
         b = :notasymbol
       end);
ERROR: ParseError: Expected type `Symbol`, got `:notasymbol` of type `QuoteNode`.
```
"""
@struct_hash_equal struct Indexed{T} <: ExprParser
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

ProxyInterfaces.dict(parser::Indexed) = parser._mapping
ProxyInterfaces.dict(::Base.Type{Indexed{T}}) where T = T
ProxyInterfaces.@dict Indexed

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
  issomething(i) && return [i]

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
