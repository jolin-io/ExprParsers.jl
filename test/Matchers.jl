using Base.Iterators
using ASTParser
using Test
const match = ASTParser.match

@testset "match plain values" begin
  @test match(5, 5) == 5
  @test_throws ParseError match(6, 7)
  @test_throws ParseError match("hi", nothing)
end

@testset "match anything" begin
  @test match(anything, 5) == 5
  @test match(anything, "hi") == "hi"
  @test match(anything, nothing) == nothing
end

@testset "match vectors" begin
  @test match([anything, anything], [1,2]) == [1,2]
  @test match([anything, 2], [1,2]) == [1,2]
  @test_throws ParseError match([anything, 3], [1,2]) == [1,2]
end

@testset "AnyOf,AllOf" begin
  @test_throws ParseError match(Matchers.AnyOf("hi", "ho", "hu"), "aa")
  @test match(Matchers.AnyOf("hi", "ho", "hu"), "hu") == "hu"
  @test match(Matchers.AllOf("a", anything), "a") == "a"
  @test_throws ParseError match(Matchers.AllOf("a", anything, "b"), "a")
end

@testset "Type" begin
  matcher = Matchers.Type(Number)
  @test match(matcher, 4) == 4
  @test match(matcher, 4.412) == 4.412
  @test match(matcher, 3//5) == 3//5
  @test_throws ParseError match(matcher, "hi")
end

@testset "Predicate" begin
  matcher = Matchers.Predicate() do x
    x in (1,3,6,7)
  end
  @test_throws ParseError match(matcher, 2)
  @test match(matcher, 6) == 6
end


@testset "Iterator" begin
  @test match(anything, [1,2,3,4]) == match(Matchers.Iterator(repeated(anything)), [1,2,3,4]) == [1,2,3,4]

  # Test Matchers.Iterator Combining
  parser = Matchers.Iterator(
    Matchers.Iterator(repeated(Matchers.Type(LineNumberNode))),
    Matchers.Iterator(repeated(Named{:nolinenumber}(anything)))
  )

  expr = quote
    2
    "hi"
  end

  parsed = parser(expr.args)
  named = [x.expr for x in parsed if x isa Parsed(Named)]
  @test named == [2, "hi"]


  # Test Matchers.Iterator plain
  # should behave exactly the same as ``parser```
  parser2 = Matchers.Iterator(repeated(Matchers.AnyOf(
    Matchers.Type(LineNumberNode),
    Named{:nolinenumber}(anything),
  )))

  parsed2 = parser2(expr.args)
  named2 = [x.expr for x in parsed2 if x isa Parsed(Named)]
  @test named2 == [2, "hi"]
end
