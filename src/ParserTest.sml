fun parse input =
  let
    val lexer = Lexer.new input
    val parser = Parser.new lexer
  in
    Parser.parseProgram parser
  end

fun testIfExpression input =
  case (parse input) of
    [AST.ExpressionStatement value] =>
      (case value of
         AST.IfExpression {condition, consequence, alternative} =>
           print ("testIfExpression:\n" ^ (AST.expToString value) ^ " OK\n")
       | _ => print ("testIfExpression " ^ input ^ " FAIL\n"))
  | _ => print ("testIfExpression " ^ input ^ " FAIL\n")

fun testFnExpression input =
  let val e = parse input
  in List.map (fn s => print (AST.toString s)) e
  end

(* case (parse input) of *)
(* [AST.ExpressionStatement {expression}] => *)
(* print "ok" *)
(* | _ => print "fail" *)

val _ =
  ( testIfExpression "if (x < y) { x }"
  ; testIfExpression "if (x < y) { x } else { y }"
  ; testFnExpression "fn (x, y) { x + y; }"
  ; testFnExpression "fn (x) { };"
  ; testFnExpression "fn (x, y) { };"
  ; testFnExpression "fn (x, y, z) { };"
  )
