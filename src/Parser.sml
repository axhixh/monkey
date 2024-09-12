structure Parser =
struct
  type ParserT =
    {lexer: Lexer.LexerT, currToken: Token.Token, peekToken: Token.Token}

  fun new lexer =
    let
      val (current, lexer) = Lexer.nextToken lexer
      val (peek, lexer) = Lexer.nextToken lexer
    in
      {lexer = lexer, currToken = current, peekToken = peek}
    end

  fun nextToken (parser: ParserT) =
    let
      val current = #currToken parser
      val peek = #peekToken parser
      val (next, lexer) = Lexer.nextToken (#lexer parser)
    in
      (current, {lexer = lexer, currToken = peek, peekToken = next})
    end


  fun parseIdentifier parser =
    let
      val (t, p) = nextToken parser
      val a =
        case t of
          Token.Ident i => i
        | _ => "unknown"
    in
      ({ident = t, value = a}, p)
    end

  fun parseValue parser =
    case (nextToken parser) of
      (Token.Int v, p) => ({identifier = "int", value = v}, p)
    | (t, p) => ({identifier = Token.toString t, value = "(todo)"}, p)


  fun parseLet parser =
    let
      val (identifier, p1) = parseIdentifier parser
      val (t, p2) = nextToken p1
      val (v, p3) = parseValue p2
    in
      (AST.Let {token = Token.Let, identifier = identifier, value = v}, p3)
    end


  fun parseFunc parser =
    let
      val (identifier, p1) = parseIdentifier parser
      val (_, p2) = nextToken p1
    in
      (AST.Func {token = Token.Function, identifier = identifier}, p2)
    end

  fun parseProgram parser =
    let
      fun parse (currentParser: ParserT) (program: AST.Program) : AST.Program =
        let
          val (t, p) = nextToken currentParser
        in
          case t of
            Token.EOF => program
          | Token.Let =>
              let
                val (stmt, p') = parseLet p
                val newProgram = stmt :: program
              in
                parse p' newProgram
              end
          | Token.Function =>
              let
                val (stmt, p') = parseFunc p
                val newProgram = stmt :: program
              in
                parse p' newProgram
              end
          | _ => program
        end
    in
      List.rev (parse parser nil)
    end
end
