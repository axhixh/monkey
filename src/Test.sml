fun test code =
  let
    fun iterate lexer =
      let
        val (t, l) = Lexer.nextToken lexer
      in
        print (Token.toString t);
        print "\n";
        if t = Token.EOF then () else iterate l
      end
  in
    iterate (Lexer.new code)
  end

val () = let val code = "let a = 30; (a + b) > (c - delta);"
         in print code; print "\n"; test code
         end
