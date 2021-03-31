using Test
using ExprParsers


# Standard Parsers
# ================

@testset "Block" begin
  parser = EP.Block(EP.anysymbol, :b, EP.Expr(:tuple, EP.anything))
  @test_throws EP.ParseError parse_expr(parser, quote
    5
    b
    (1,4,a)
  end)

  test_closure(parser, quote
    a
    b
    (1,4,a)
  end)

  parsed = parse_expr(EP.Block(), quote
    f(a) = A
    3
  end)
  parsed_exprs = filter(x -> !isa(x, LineNumberNode), parsed.exprs)
  @test parsed_exprs[1].head == :(=)
  @test parsed_exprs[1].args[1] == :(f(a))
  @test parsed_exprs[2] == 3
end

@testset "Arg" begin
  parser = EP.Arg()
  parsed = parse_expr(parser, Expr(:kw, :a, 4))
  @test parsed.name == :a
  @test parsed.type == Any
  @test parsed.default == 4
  test_closure(parser, Expr(:kw, :a, 4))

  @test_throws EP.ParseError parse_expr(parser, :(a = 5))

  expr = :(f(a::Int, b=5; c, d=6) = a + b + c + d)
  expr.args[1].args[3:end][2].head
  a, b = [parse_expr(parser, x) for x in expr.args[1].args[3:end]]
  c, d = [parse_expr(parser, x) for x in expr.args[1].args[2].args]
  @test a.name == :a
  @test a.default == EP.nodefault
  @test a.type == :Int

  @test b.name == :b
  @test b.default == 5
  @test b.type == Any

  @test c.name == :c
  @test c.default == EP.nodefault
  @test c.type == Any

  @test d.name == :d
  @test d.default == 6
  @test d.type == Any

  @testset "Vararg" begin
    expr = :(f(args...; kwargs...) = (args, kwargs))
    args_expr = expr.args[1].args[3]
    kwargs_expr = expr.args[1].args[2].args[1]
    args = parse_expr(parser, args_expr)
    kwargs = parse_expr(parser, kwargs_expr)
    @test args.type === Vararg
    @test args.name === :args
    @test args.default === EP.nodefault
    @test kwargs.type === Vararg
    @test kwargs.name === :kwargs
    @test kwargs.default === EP.nodefault

    test_closure(parser, args_expr)
    test_closure(parser, kwargs_expr)
  end

  parsed = EP.Arg_Parsed(name = :a)
  @test parse_expr(EP.Arg(), to_expr(parsed)) == parsed
end

@testset "Module" begin
  parser = EP.Module()
  expr = :(module A
    something
    3
  end)
  parsed = parse_expr(parser, expr)
  @test parsed.no_bare == true
  @test parsed.name == :A
  @test filter(x -> !isa(x, LineNumberNode), parse_expr(EP.Block(), parsed.body).exprs) == [:something, 3]

  test_closure(parser, expr)

  @test_throws EP.ParseError parse_expr(parser, :(a::Int))
  @test_throws EP.ParseError parse_expr(parser, quote
      module A
    end
  end)
end

@testset "Assignment" begin
  parser = EP.Assignment()
  parsed = parse_expr(parser, :(a::Int = 4))
  @test parsed.left == :(a::Int)
  @test parsed.right == 4

  test_closure(parser, :(a::Int = 4))

  @test_throws EP.ParseError parse_expr(parser, :(a::Int))

  parsed = EP.Assignment_Parsed(left = :left, right = :right)
  @test parse_expr(EP.Assignment(), to_expr(parsed)) == parsed
end


@testset "Call" begin
  parser = EP.Call()
  parsed = parse_expr(parser, :(f{A, B}(a, b, c...; d...)))
  @test parsed.name == :f
  @test parsed.curlies == [:A, :B]
  @test parsed.args == [:a, :b, :(c...)]
  @test parsed.kwargs == [:(d...)]

  test_closure(parser, :(f{A, B}(a, b, c...; d...)))

  @test_throws EP.ParseError parse_expr(parser, :(a = 4))

  parsed = EP.Call_Parsed(name = :f)
  @test parse_expr(EP.Call(), to_expr(parsed)) == parsed
end

@testset "Expr" begin
  expr = Expr(:head, :a1, 2, [3,4])
  parsed_expr = EP.Expr_Parsed(:head, [:a1, 2, [3,4]])
  @test parse_expr(EP.Expr(), expr) == parsed_expr
  @test_throws EP.ParseError parse_expr(EP.Expr(head = EP.AnyOf(:hi, :ho)), expr)
  @test parse_expr(EP.Expr(head = EP.AnyOf(:hi, :ho, :head)), expr) == parsed_expr
  @test_throws EP.ParseError parse_expr(EP.Expr(args = [1,2]), expr)
  @test parse_expr(EP.Expr(args = [:a1, 2, [3,4]]), expr) == parsed_expr
  @test parse_expr(EP.Expr(args = [:a1, 2, EP.anything]), expr) == parsed_expr

  # All Parsers should be Closure operators
  test_closure(EP.Expr(), expr)

  # second constructor
  parser = EP.Expr(quote
    a = 4
    b = 5
  end)
  test_closure(parser, quote
    a = 4
    b = 5
  end)

  # Expr should also support nested Parsers
  parser = EP.Expr(quote
    a = $(EP.Isa(Int))
    b = $(EP.anysymbol)
  end)
  parsed = parse_expr(parser, quote
    a = 4
    b = a
  end)

  test_closure(parser, quote
    a = 4
    b = a
  end)

  @test parsed.args.exprs[4].args[2] isa Base.Symbol

  @test_throws EP.ParseError parse_expr(parser, quote
    a = 4.0
    b = a
  end)
end

@testset "Signature" begin
  parser = EP.Signature()

  expr = :(
    f(a::String, b::Int...; c = 5, d...)
  )
  parsed = parse_expr(parser, expr)
  @test parsed.name == :f
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == []
  @test parsed.wheres == []
  test_closure(parser, expr)

  expr = :(
    (a::String, b::Int...; c = 5, d...) where B where A
  )
  parsed = parse_expr(parser, expr)
  @test parsed.name == nothing
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == []
  @test parsed.wheres == [:A, :B]
  test_closure(parser, expr)

  expr = :(
    f{A}(a::String, b::Int...; c = 5, d...) where {A, B}
  )
  parsed = parse_expr(parser, expr)
  @test parsed.name == :f
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == [:A]
  @test parsed.wheres == [:A, :B]
  test_closure(parser, expr)

  @test_throws EP.ParseError parse_expr(parser, :(
    f = 5
  ))

  parsed = EP.Signature_Parsed()
  @test parse_expr(EP.Signature(), to_expr(parsed)) == parsed
end

@testset "Function" begin
  parser = EP.Function()

  expr = :(
    function f(a::String, b::Int...; c = 5, d...)
    end
  )
  parsed = parse_expr(parser, expr)
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
  parsed = parse_expr(parser, expr)
  @test parsed.name == nothing
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == []
  @test parsed.wheres == [:A, :B]
  test_closure(parser, expr)

  expr = :(
    f{A}(a::String, b::Int...; c = 5, d...) where {A, B} = nothing
  )
  parsed = parse_expr(parser, expr)
  @test parsed.name == :f
  @test parsed.args == [:(a::String), :(b::Int...)]
  @test parsed.kwargs == [Expr(:kw, :c, 5), :(d...)]
  @test parsed.curlies == [:A]
  @test parsed.wheres == [:A, :B]
  test_closure(parser, expr)

  @test_throws EP.ParseError parse_expr(parser, :(
    f = 5
  ))

  parsed = EP.Function_Parsed()
  @test parse_expr(EP.Function(), to_expr(parsed)) == parsed
end


@testset "Macro" begin
  parser = EP.Macro()
  expr = :(@mymacro whatever 3)
  parsed = parse_expr(parser, expr)
  @test parsed.name == :mymacro
  @test parsed.args == [:whatever, 3]
  test_closure(parser, expr)

  parsed = EP.Macro_Parsed(name = :mymcaro)
  @test parse_expr(EP.Macro(), to_expr(parsed)) == parsed
end

@testset "NestedDot" begin
  parser = EP.NestedDot()
  expr = :(A{T}("hi").b.c.d)
  parsed = parse_expr(parser, expr)
  @test parsed.base == :(A{T}("hi"))
  @test parsed.properties == [:b, :c, :d]
  test_closure(parser, expr)

  @test_throws EP.ParseError parse_expr(parser, :(A{hi}))

  parsed = EP.NestedDot_Parsed(base = :a, properties = [:b, :c])
  @test parse_expr(EP.NestedDot(), to_expr(parsed)) == parsed
end

@testset "Reference" begin
  parser = EP.Reference()
  @test parse_expr(parser, :a).name == :a
  expr = :(A{T, S})
  parsed = parse_expr(parser, expr)
  @test parsed.name == :A
  @test parsed.curlies == [:T, :S]
  test_closure(parser, expr)

  expr = :(A.B.C{T, S})
  parsed = parse_expr(parser, expr)
  @test parsed.name == :(A.B.C)
  @test parsed.curlies == [:T, :S]
  test_closure(parser, expr)

  expr = :(A.B.C)
  parsed = parse_expr(parser, expr)
  @test parsed.name == :(A.B.C)
  @test parsed.curlies == []
  test_closure(parser, expr)

  @test_throws EP.ParseError parse_expr(parser, :(f(a, b)))

  parsed = EP.Reference_Parsed(name = :a)
  @test parse_expr(EP.Reference(), to_expr(parsed)) == parsed
end


@testset "Type" begin
  parser = EP.Type()
  expr = Any
  @test parse_expr(parser, expr).name == Any
  @test parse_expr(parser, expr).curlies == []
  @test parse_expr(parser, expr).wheres == []
  test_closure(parser, expr)

  expr = :Any
  @test parse_expr(parser, expr).name == :Any
  @test parse_expr(parser, expr).curlies == []
  @test parse_expr(parser, expr).wheres == []
  test_closure(parser, expr)

  expr = :(A{T, S})
  @test parse_expr(parser, expr).name == :A
  @test parse_expr(parser, expr).curlies == [:T, :S]
  @test parse_expr(parser, expr).wheres == []
  test_closure(parser, expr)

  expr = :(A{T, S} where S where T)
  @test parse_expr(parser, expr).name == :A
  @test parse_expr(parser, expr).curlies == [:T, :S]
  @test parse_expr(parser, expr).wheres == [:T, :S]
  test_closure(parser, expr)

  @test_throws EP.ParseError parse_expr(parser, :(f(a, b)))

  parsed = EP.Type_Parsed(name = :a)
  @test parse_expr(EP.Type(), to_expr(parsed)) == parsed
end

@testset "TypeAnnotation" begin
  parser = EP.TypeAnnotation()
  expr = :(::Any)
  @test parse_expr(parser, expr).name == nothing
  @test parse_expr(parser, expr).type == :Any
  test_closure(parser, expr)

  expr = :(something(hi)::Any)
  @test parse_expr(parser, expr).name == :(something(hi))
  @test parse_expr(parser, expr).type == :Any
  test_closure(parser, expr)

  @test_throws EP.ParseError parse_expr(parser, :a)

  parsed = EP.TypeAnnotation_Parsed(name = :a, type = Any)
  @test parse_expr(EP.TypeAnnotation(), to_expr(parsed)) == parsed
end


@testset "TypeRange" begin
  parser = EP.TypeRange()
  expr = :(A <: Any)
  @test parse_expr(parser, expr).lb == Union{}
  @test parse_expr(parser, expr).name == :A
  @test parse_expr(parser, expr).ub == :Any
  test_closure(parser, expr)

  expr = :(A >: Integer)
  @test parse_expr(parser, expr).lb == :Integer
  @test parse_expr(parser, expr).name == :A
  @test parse_expr(parser, expr).ub == Any
  test_closure(parser, expr)

  expr = :(Number <: B <: Any)
  @test parse_expr(parser, expr).lb == :Number
  @test parse_expr(parser, expr).name == :B
  @test parse_expr(parser, expr).ub == :Any
  test_closure(parser, expr)

  @test_throws EP.ParseError parse_expr(parser, :(a = 4))

  parsed = EP.TypeRange_Parsed(name = :T)
  @test parse_expr(EP.TypeRange(), to_expr(parsed)) == parsed
end
