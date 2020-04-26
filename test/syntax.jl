@exprparser struct MySymbol
  symbol = EP.anything
end
@test MySymbol <: EP.ExprParser
@test MySymbol <: EP.ExprParserWithParsed
@test MySymbol_Parsed <: EP.ExprParsed

@test EP.ExprParsed(MySymbol) === MySymbol_Parsed

@test parse_expr(MySymbol(), symbol = 4) == MySymbol_Parsed(symbol = 4)
