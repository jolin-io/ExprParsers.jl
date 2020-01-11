@parserfactory struct MySymbol
  symbol = anything
end
@test MySymbol <: Parser
@test Parsed(MySymbol) <: Parsed

@test MySymbol()(symbol = 4) == Parsed(MySymbol)(symbol = 4)
