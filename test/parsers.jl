__precompile__(false) #REMOVE
# ## Testing our parser
module ParserTests
using Test
using SyntacticModels
using SyntacticModels.ASKEMUWDs
using SyntacticModels.Parsers
using Catlab

# Now we write some unit tests. This is how I wrote this code, by writing the tests from the bottom up.

@testset "Parens" begin
  @test lparen("(")[1] == "("
  @test rparen(")")[1] == ")"
  @test elname("R(a)")[1] == "R"
end

@testset "Judgements" begin
  @test judgement("a:A,")[1] == Typed(:a, :A)
  @test judgement("ab:AB,")[1] == Typed(:ab, :AB)

  @test finjudgement("a:A")[1] == Typed(:a, :A)
  @test finjudgement("ab:AB")[1] == Typed(:ab, :AB)
end

@testset "Contexts" begin
  @test Parsers.context("{a:A,b:B}")[1] == [Typed(:a, :A), Typed(:b, :B)]
end


@testset "Statements" begin
  @test [Untyped(:u)] == [Untyped(:u)]
  @test statement("R(a,b)")[1] == Statement(:R, [Untyped(:a),Untyped(:b)])
  @test statement("S(u,b)")[1] == Statement(:S, [Untyped(:u),Untyped(:b)])
  @test statement("S(u,b,x)")[1].relation == Statement(:S, [Untyped(:u), Untyped(:b), Untyped(:x)]).relation
  @test statement("S(u,b,x)")[1].variables == Statement(:S, [Untyped(:u), Untyped(:b), Untyped(:x)]).variables
  @test statement("S(u)")[1].relation == Statement(:S, [Untyped(:u)]).relation
  @test statement("S(u)")[1].variables == Statement(:S, Var[Untyped(:u)]).variables
end

@testset "Body" begin
  @test body("""{
  R(a,b);}""")[1][1] isa Statement

  @test body("""{
  R(a,b);
  }""")[1][1] isa Statement

  @test body("""{
    R(a,b);
  }""")[1][1] isa Statement

  @test length(body("""{
  R(a,b);
    S(u,b);
  }""")[1]) == 2
end

# Our final test shows that we can parse what we expect to be able to parse:
@testset "UWD" begin
  @test uwd("""{R(a,b); S(b,c);} where {a:A,b:B,c:C}""")[1].context == [Typed(:a, :A), Typed(:c,:C)]
  @test uwd("""{R(a,b); S(b,c);}
   where {a:A,b:B,c:C}""")[1].statements == [Statement(:R, [Typed(:a, :A), Typed(:b, :B)]),
    Statement(:S, [Typed(:b, :B), Typed(:c, :C)])]
  @test uwd("""{R(a,b); S(b,c);} where {a:A,b:B,c:C}""")[1] isa ASKEMUWDs.UWDExpr
end

# End-To-End Test Cases illustrating full on use of string macro
@testset "End-To-End" begin
  parsed_result = relation"{R(x,y); S(y,z);} where {x:X,y:Y,z:Z}"
  
  v1 = Typed(:x, :X)
  v2 = Typed(:y, :Y)
  v3 = Typed(:z, :Z)
  c = [v1, v3]
  s = [Statement(:R, [v1,v2]),
    Statement(:S, [v2,v3])]
  u = UWDExpr(c, s)
  uwd_result = ASKEMUWDs.construct(RelationDiagram, u)
  
  @test parsed_result == uwd_result

end
end


#Notes:
# Outerport creation seems to be broken
# Typing creates untyped "Typed" objects which technically differs.
# This is occuring cause we make our outerports based on the context
# For some reason this variation of the relation macro ignores defining outerports first.