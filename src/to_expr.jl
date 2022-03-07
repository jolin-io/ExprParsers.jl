"""
collection of convenience functions for `to_expr` for Data Structures from Base 
which occur relatively often in the context of parsing and macros
"""



function ExprParsers.to_expr(tv::TypeVar)
    if tv.lb === Union{} && tv.ub === Any
        tv.name
    elseif tv.lb === Union{}
        :($(tv.name) <: $(tv.ub))
    elseif tv.ub === Any
        :($(tv.name) >: $(tv.lb))
    else
        :($(tv.lb) <: $(tv.name) <: $(tv.ub))
    end
end