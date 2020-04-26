
# Meta Parsers
# ============

@testset "Named" begin
  parser = EP.Named{:hi}(EP.anysymbol)
  parsed = parse_expr(parser, :symbol)
  @test parsed isa EP.Named_Parsed{:hi}
  test_closure(parser, :symbol)
end

@testset "Indexed" begin
  indexed_parser = EP.Indexed() do dict
    EP.Expr(quote
      a = $(dict[:a] = EP.Isa(Int))
      b = $(dict[:b] = EP.anysymbol)
    end)
  end
  expr = quote
    a = 4
    b = a
  end
  test_closure(indexed_parser, expr)
  indexed_parsed = parse_expr(indexed_parser, expr)
  @test collect(keys(indexed_parsed)) == [:a; :b]
  @test collect(values(indexed_parsed)) == [4, :a]
  @test indexed_parsed[:a] == 4
  @test indexed_parsed[:b] == :a

  indexed_parsed[:a] = 5
  @test indexed_parsed[:a] == 5
  to_expr(indexed_parsed)  # looks good, but hard to test automatically

  @test_throws ParseError parse_expr(indexed_parser, quote
    a = 4.0
    b = a
  end)
end
