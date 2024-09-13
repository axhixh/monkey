fun readLine input =
  Option.getOpt (TextIO.inputLine input, "")

fun writeLine output s =
  (TextIO.output (output, s); TextIO.flushOut output)

fun writeStatement output stmt =
  (writeLine output (AST.toString stmt); stmt)

fun writeProgram output program =
  case program of
  NONE => writeLine output "unable to parse program"
  | SOME prgrm => (List.map (writeStatement output) prgrm; writeLine output "done")

fun start input output =
  let
    val code = (writeLine output ">>>"; readLine input)
    val lexer = Lexer.new code
    val parser = Parser.new lexer
    val program = Parser.parseProgram parser
  in
    writeProgram output program
  end

val _ =
  let
    val output = TextIO.stdOut
    val input = TextIO.stdIn
  in
    (writeLine output "Starting Monkey REPL\n"; start input output)
  end
