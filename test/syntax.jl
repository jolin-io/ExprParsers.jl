@exprparser struct MySymbol
  symbol = anything
end
@test MySymbol <: ExprParser
@test MySymbol_Parsed <: ExprParsed

@test ExprParsed(MySymbol) === MySymbol_Parsed

@test parse_expr(MySymbol(), symbol = 4) == MySymbol_Parsed(symbol = 4)
