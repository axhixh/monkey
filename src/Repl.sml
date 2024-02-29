fun readLine input =
  Option.getOpt (TextIO.inputLine input, "")

fun writeLine output s =
  (TextIO.output (output, s); TextIO.flushOut output)

fun writeTokens output lexer =
  let
    val (t, l) = Lexer.nextToken lexer
  in
    case t of
      Token.EOF => writeLine output "\n"
    | _ =>
        ( writeLine output (Token.toString t)
        ; writeLine output " "
        ; writeTokens output l
        )
  end

fun start input output =
  let
    val code = (writeLine output ">>>"; readLine input)
    val lexer = Lexer.new code
  in
    (writeTokens output lexer; start input output)
  end

val _ =
  let
    val output = TextIO.stdOut
    val input = TextIO.stdIn
  in
    (writeLine output "Starting Monkey REPL\n"; start input output)
  end
