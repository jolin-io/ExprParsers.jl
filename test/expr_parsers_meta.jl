
# Meta Parsers
# ============

@testset "Named" begin
  parser = Named{:hi}(EP.Symbol())
  parsed = parse_expr(parser, :symbol)
  @test parsed isa Named_Parsed{:hi}
  test_closure(parser, :symbol)
end

@testset "Indexed" begin
  indexed_parser = Indexed() do dict
    EP.Expr(quote
      a = $(dict[:a] = EP.Isa(Int))
      b = $(dict[:b] = EP.Symbol())
    end)
  end
  test_closure(indexed_parser, quote
    a = 4
    b = a
  end)
  indexed_parsed = parse_expr(indexed_parser, quote
    a = 4
    b = a
  end)
  @test collect(keys(indexed_parsed)) == [:a; :b]
  @test collect(values(indexed_parsed)) == [4, EP.Symbol_Parsed(:a)]
  @test indexed_parsed[:a] == 4
  @test indexed_parsed[:b].symbol == :a
  # FUTURE: we may want to implement setindex! too

  @test_throws ParseError parse_expr(indexed_parser, quote
    a = 4.0
    b = a
  end)
end
