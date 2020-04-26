using Test
using ExprParsers

@testset "syntax" begin
  include("syntax.jl")
end

@testset "simple_parsers" begin
  include("simple_parsers.jl")
end

@testset "expr_parsers" begin
  include("expr_parsers.jl")
end
