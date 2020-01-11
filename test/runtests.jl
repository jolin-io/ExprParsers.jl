using Test
using ASTParser

@testset "Matcher" begin
  include("Matchers.jl")
end

@testset "Parsers" begin
  include("Parsers/Parsers.jl")
end
