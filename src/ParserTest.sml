fun testIfExpression input =
  let
    val lexer = Lexer.new input
    val parser = Parser.new lexer
    val program: AST.Program = Parser.parseProgram parser
  in
    case program of
      [AST.ExpressionStatement {token, value}] =>
        (case value of
           AST.IfExpression {token, condition, consequence, alternative} =>
             print ("testIfExpression:\n" ^ (AST.expToString value) ^ " OK\n")
         | _ => print ("testIfExpression " ^ input ^ " FAIL\n"))
    | _ => print ("testIfExpression " ^ input ^ " FAIL\n")
  end

val _ =
  ( testIfExpression "if (x < y) { x }"
  ; testIfExpression "if (x < y) { x } else { y }"
  )
