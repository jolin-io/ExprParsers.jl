using Test
using ExprParsers

"""
  testing that `parser(parser(expr)) == parser(expr)`

which is the property of a closure operator, hence the name
"""
function test_closure(parser, expr)
  parsed_expr′ = parse_expr(parser, expr)
  expr′ = to_expr(parsed_expr′)
  parsed_expr′′ = parse_expr(parser, expr′)
  expr′′ = to_expr(parsed_expr′′)
  @test parsed_expr′ == parsed_expr′′
  @test expr′ == expr′′
end


@testset "syntax" begin
  include("syntax.jl")
end

@testset "expr_parsers_core" begin
  include("expr_parsers_core.jl")
end

@testset "expr_parsers_with_parsed" begin
  include("expr_parsers_with_parsed.jl")
end

@testset "expr_parsers_meta" begin
  include("expr_parsers_meta.jl")
end
