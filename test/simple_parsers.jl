using Base.Iterators
using ExprParsers
using Test

@testset "parse_expr plain values" begin
  @test parse_expr(5, 5) == 5
  @test_throws ParseError parse_expr(6, 7)
  @test_throws ParseError parse_expr("hi", nothing)
end

@testset "parse_expr anything" begin
  @test parse_expr(anything, 5) == 5
  @test parse_expr(anything, "hi") == "hi"
  @test parse_expr(anything, nothing) == nothing
end

@testset "parse_expr vectors" begin
  @test parse_expr([anything, anything], [1,2]) == [1,2]
  @test parse_expr([anything, 2], [1,2]) == [1,2]
  @test_throws ParseError parse_expr([anything, 3], [1,2]) == [1,2]
end

@testset "AnyOf,AllOf" begin
  @test_throws ParseError parse_expr(AnyOf("hi", "ho", "hu"), "aa")
  @test parse_expr(AnyOf("hi", "ho", "hu"), "hu") == "hu"
  @test parse_expr(AllOf("a", anything), "a") == "a"
  @test_throws ParseError parse_expr(AllOf("a", anything, "b"), "a")
end

@testset "Isa" begin
  matcher = EP.Isa(Number)
  @test parse_expr(matcher, 4) == 4
  @test parse_expr(matcher, 4.412) == 4.412
  @test parse_expr(matcher, 3//5) == 3//5
  @test_throws ParseError parse_expr(matcher, "hi")
end

@testset "SatisfiesPredicate" begin
  matcher = EP.SatisfiesPredicate() do x
    x in (1,3,6,7)
  end
  @test_throws ParseError parse_expr(matcher, 2)
  @test parse_expr(matcher, 6) == 6
end


@testset "Iterator" begin
  @test parse_expr(anything, [1,2,3,4]) == parse_expr(Iterator(repeated(anything)), [1,2,3,4]) == [1,2,3,4]
end
