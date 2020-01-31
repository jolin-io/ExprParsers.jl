using ASTParser
using Test

@testset "utils" begin
  include("utils.jl")
end

@testset "syntax" begin
  include("syntax.jl")
end

"""
  testing that `parser(parser(expr)) == parser(expr)`

which is the property of a closure operator, hence the name
"""
function test_closure(parser, expr)
  parsed_expr′ = parser(expr)
  expr′ = toAST(parsed_expr′)
  parsed_expr′′ = parser(expr′)
  expr′′ = toAST(parsed_expr′′)
  @test parsed_expr′ == parsed_expr′′
  @test expr′ == expr′′
end


# Meta Parsers
# ============

@testset "Named" begin
  parser = Named{:hi}(Parsers.Symbol())
  parsed = parser(:symbol)
  @test parsed isa Named_Parsed{:hi}
  test_closure(parser, :symbol)
end

@testset "Indexed" begin
  indexed_parser = Indexed() do dict
    Parsers.Expr(quote
      a = $(dict[:a] = Matchers.Type(Int))
      b = $(dict[:b] = Parsers.Symbol())
    end)
  end
  test_closure(indexed_parser, quote
    a = 4
    b = a
  end)
  indexed_parsed = indexed_parser(quote
    a = 4
    b = a
  end)
  @test collect(keys(indexed_parsed)) == [:a; :b]
  @test collect(values(indexed_parsed)) == [4, Parsers.Symbol_Parsed(:a)]
  @test indexed_parsed[:a] == 4
  @test indexed_parsed[:b].symbol == :a
  # FUTURE: we may want to implement setindex! too

  @test_throws ParseError indexed_parser(quote
    a = 4.0
    b = a
  end)
end

# Standard Parsers
# ================

@testset "Block" begin
  parser = Parsers.Block(Parsers.Symbol(), :b, Parsers.Expr(:tuple, anything))
  @test_throws ParseError parser(quote
    5
    b
    (1,4,a)
  end)

  test_closure(parser, quote
    a
    b
    (1,4,a)
  end)
end

@testset "Arg" begin
  parser = Parsers.Arg()
  parsed = parser(Expr(:kw, :a, 4))
  @test parsed.name == :a
  @test parsed.type == Any
  @test parsed.default == 4
  test_closure(parser, Expr(:kw, :a, 4))

  @test_throws ParseError parser(:(a = 5))

  expr = :(f(a::Int, b=5; c, d=6) = a + b + c + d)
  expr.args[1].args[3:end][2].head
  a, b = parser.(expr.args[1].args[3:end])
  c, d = parser.(expr.args[1].args[2].args)
  @test a.name == :a
  @test a.default == nodefault
  @test a.type == :Int

  @test b.name == :b
  @test b.default == 5
  @test b.type == Any

  @test c.name == :c
  @test c.default == nodefault
  @test c.type == Any

  @test d.name == :d
  @test d.default == 6
  @test d.type == Any

  @testset "Vararg" begin
    expr = :(f(args...; kwargs...) = (args, kwargs))
    args_expr = expr.args[1].args[3]
    kwargs_expr = expr.args[1].args[2].args[1]
    args = parser(args_expr)
    kwargs = parser(kwargs_expr)
    @test args.type === Vararg
    @test args.name === :args
    @test args.default === nodefault
    @test kwargs.type === Vararg
    @test kwargs.name === :kwargs
    @test kwargs.default === nodefault

    test_closure(parser, args_expr)
    test_closure(parser, kwargs_expr)
  end
end


@testset "Assignment" begin
  parser = Parsers.Assignment()
  parsed = parser(:(a::Int = 4))
  @test parsed.left == :(a::Int)
  @test parsed.right == 4

  test_closure(parser, :(a::Int = 4))

  @test_throws ParseError parser(:(a::Int))
end


@testset "Call" begin
  parser = Parsers.Call()
  parsed = parser(:(f{A, B}(a, b, c...; d...)))
  @test parsed.name == :f
  @test parsed.curlies == [:A, :B]
  @test parsed.args == [:a, :b, :(c...)]
  @test parsed.kwargs == [:(d...)]

  test_closure(parser, :(f{A, B}(a, b, c...; d...)))

  @test_throws ParseError parser(:(a = 4))
end

@testset "Expr" begin
  expr = Expr(:head, :a1, 2, [3,4])
  parsed_expr = Parsed(Parsers.Expr)(:head, [:a1, 2, [3,4]])
  @test Parsers.Expr()(expr) == parsed_expr
  @test_throws ParseError Parsers.Expr(head = Matchers.AnyOf(:hi, :ho))(expr)
  @test Parsers.Expr(head = Matchers.AnyOf(:hi, :ho, :head))(expr) == parsed_expr
  @test_throws ParseError Parsers.Expr(args = [1,2])(expr)
  @test Parsers.Expr(args = [:a1, 2, [3,4]])(expr) == parsed_expr
  @test Parsers.Expr(args = [:a1, 2, anything])(expr) == parsed_expr

  # All Parsers should be Closure operators
  test_closure(Parsers.Expr(), expr)


  # second constructor
  parser = Parsers.Expr(quote
    a = 4
    b = 5
  end)
  test_closure(parser, quote
    a = 4
    b = 5
  end)

  # Expr should also support nested Parsers
  parser = Parsers.Expr(quote
    a = $(Matchers.Type(Int))
    b = $(Parsers.Symbol())
  end)
  parsed = parser(quote
    a = 4
    b = a
  end)

  test_closure(parser, quote
    a = 4
    b = a
  end)

  @test parsed.args.exprs[4].args[2] isa Parsers.Symbol_Parsed

  @test_throws ParseError parser(quote
    a = 4.0
    b = a
  end)
end

@testset "Signature" begin
  parser = Parsers.Signature()

  expr = :(
    f(a::String, b::Int...; c = 5, d...)
  )
  parsed = parser(expr)
  @test parsed.name == :f
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == []
  @test parsed.wheres == []
  test_closure(parser, expr)

  expr = :(
    (a::String, b::Int...; c = 5, d...) where B where A
  )
  parsed = parser(expr)
  @test parsed.name == nothing
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == []
  @test parsed.wheres == [:A, :B]
  test_closure(parser, expr)

  expr = :(
    f{A}(a::String, b::Int...; c = 5, d...) where {A, B}
  )
  parsed = parser(expr)
  @test parsed.name == :f
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == [:A]
  @test parsed.wheres == [:A, :B]
  test_closure(parser, expr)

  @test_throws ParseError parser(:(
    f = 5
  ))
end

@testset "Function" begin
  parser = Parsers.Function()

  expr = :(
    function f(a::String, b::Int...; c = 5, d...)
    end
  )
  parsed = parser(expr)
  @test parsed.name == :f
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == []
  @test parsed.wheres == []
  test_closure(parser, expr)

  expr = :(
    function (a::String, b::Int...; c = 5, d...) where B where A
    end
  )
  parsed = parser(expr)
  @test parsed.name == nothing
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == []
  @test parsed.wheres == [:A, :B]
  test_closure(parser, expr)

  expr = :(
    f{A}(a::String, b::Int...; c = 5, d...) where {A, B} = nothing
  )
  parsed = parser(expr)
  @test parsed.name == :f
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == [:A]
  @test parsed.wheres == [:A, :B]
  test_closure(parser, expr)

  @test_throws ParseError parser(:(
    f = 5
  ))
end


@testset "Macro" begin
  parser = Parsers.Macro()
  expr = :(@mymacro whatever 3)
  parsed = parser(expr)
  @test parsed.name == :mymacro
  @test parsed.args == [:whatever, 3]
  test_closure(parser, expr)
end

@testset "NestedDot" begin
  parser = Parsers.NestedDot()
  expr = :(A{T}("hi").b.c.d)
  parsed = parser(expr)
  @test parsed.base == :(A{T}("hi"))
  @test parsed.properties == [:b, :c, :d]
  test_closure(parser, expr)

  @test_throws ParseError parser(:(A{hi}))
end

@testset "Reference" begin
  parser = Parsers.Reference()
  @test parser(:a).name == :a
  expr = :(A{T, S})
  parsed = parser(expr)
  @test parsed.name == :A
  @test parsed.curlies == [:T, :S]
  test_closure(parser, expr)

  expr = :(A.B.C{T, S})
  parsed = parser(expr)
  @test parsed.name == :(A.B.C)
  @test parsed.curlies == [:T, :S]
  test_closure(parser, expr)

  expr = :(A.B.C)
  parsed = parser(expr)
  @test parsed.name == :(A.B.C)
  @test parsed.curlies == []
  test_closure(parser, expr)

  @test_throws ParseError parser(:(f(a, b)))
end


@testset "Symbol" begin
  @test toAST(Parsers.Symbol()(:hi)) == :hi
  @test_throws ParseError Parsers.Symbol()("ho")
  @test toAST(Parsers.Symbol(symbol = :thisone)(:thisone)) == :thisone
  @test_throws ParseError Parsers.Symbol(symbol = :thisone)(:hi)

  # All Parsers should be Closure operators
  test_closure(Parsers.Symbol(), :test)
end

@testset "Type" begin
  parser = Parsers.Type()
  expr = Any
  @test parser(expr).name == Any
  @test parser(expr).curlies == []
  @test parser(expr).wheres == []
  test_closure(parser, expr)

  expr = :Any
  @test parser(expr).name == :Any
  @test parser(expr).curlies == []
  @test parser(expr).wheres == []
  test_closure(parser, expr)

  expr = :(A{T, S})
  @test parser(expr).name == :A
  @test parser(expr).curlies == [:T, :S]
  @test parser(expr).wheres == []
  test_closure(parser, expr)

  expr = :(A{T, S} where S where T)
  @test parser(expr).name == :A
  @test parser(expr).curlies == [:T, :S]
  @test parser(expr).wheres == [:T, :S]
  test_closure(parser, expr)

  @test_throws ParseError parser(:(f(a, b)))
end

@testset "TypeAnnotation" begin
  parser = Parsers.TypeAnnotation()
  expr = :(::Any)
  @test parser(expr).name == nothing
  @test parser(expr).type == :Any
  test_closure(parser, expr)

  expr = :(something(hi)::Any)
  @test parser(expr).name == :(something(hi))
  @test parser(expr).type == :Any
  test_closure(parser, expr)

  @test_throws ParseError parser(:a)
end


@testset "TypeRange" begin
  parser = Parsers.TypeRange()
  expr = :(A <: Any)
  @test parser(expr).lb == Union{}
  @test parser(expr).name == :A
  @test parser(expr).ub == :Any
  test_closure(parser, expr)

  expr = :(A >: Integer)
  @test parser(expr).lb == :Integer
  @test parser(expr).name == :A
  @test parser(expr).ub == Any
  test_closure(parser, expr)

  expr = :(Number <: B <: Any)
  @test parser(expr).lb == :Number
  @test parser(expr).name == :B
  @test parser(expr).ub == :Any
  test_closure(parser, expr)

  @test_throws ParseError parser(:a)
end
