using Base.Iterators
using ExprParsers
using Test

@testset "parse_expr plain values" begin
  @test parse_expr(5, 5) == 5
  @test_throws EP.ParseError parse_expr(6, 7)
  @test_throws EP.ParseError parse_expr("hi", nothing)
end

@testset "parse_expr EP.anything" begin
  @test parse_expr(EP.anything, 5) == 5
  @test parse_expr(EP.anything, "hi") == "hi"
  @test parse_expr(EP.anything, nothing) == nothing
end

@testset "parse_expr vectors" begin
  @test parse_expr([EP.anything, EP.anything], [1,2]) == [1,2]
  @test parse_expr([EP.anything, 2], [1,2]) == [1,2]
  @test_throws EP.ParseError parse_expr([EP.anything, 3], [1,2]) == [1,2]
end

@testset "AnyOf,AllOf" begin
  @test_throws EP.ParseError parse_expr(EP.AnyOf("hi", "ho", "hu"), "aa")
  @test parse_expr(EP.AnyOf("hi", "ho", "hu"), "hu") == "hu"
  @test parse_expr(EP.AllOf("a", EP.anything), "a") == "a"
  @test_throws EP.ParseError parse_expr(EP.AllOf("a", EP.anything, "b"), "a")
end

@testset "Isa" begin
  matcher = EP.Isa(Number)
  @test parse_expr(matcher, 4) == 4
  @test parse_expr(matcher, 4.412) == 4.412
  @test parse_expr(matcher, 3//5) == 3//5
  @test_throws EP.ParseError parse_expr(matcher, "hi")
end

@testset "SatisfiesPredicate" begin
  matcher = EP.SatisfiesPredicate() do x
    x in (1,3,6,7)
  end
  @test_throws EP.ParseError parse_expr(matcher, 2)
  @test parse_expr(matcher, 6) == 6
end


@testset "Iterator" begin
  @test parse_expr(EP.anything, [1,2,3,4]) == parse_expr(EP.Utils.Iterator(repeated(EP.anything)), [1,2,3,4]) == [1,2,3,4]
end
