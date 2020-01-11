
struct A
  a
  a2
end

struct B
  b
end

struct C end

c = C()
nested = A(B([1, 5, "hi", c]), 6)

@test ASTParser.Parsers.get_object(ASTParser.Parsers.find_object(c, nested), nested) === c