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
    case (nextToken parser) of
      (Token.Ident i, p) => SOME ({ident = Token.Ident i, value = i}, p)
    | _ => NONE

  fun parseAssign parser =
    case (nextToken parser) of
      (Token.Assign, p) => SOME p
    | _ => NONE


  fun parseExpression parser =
    case (nextToken parser) of
      (Token.Int v, p) => ({identifier = "int", value = v}, p)
    | (t, p) => ({identifier = Token.toString t, value = "(todo)"}, p)


  fun parseLet parser =
    let
      fun build (id, p1) =
        Option.map
          (fn p2 =>
             let val (v, p3) = parseExpression p2
             in (AST.Let {token = Token.Let, identifier = id, value = v}, p3)
             end) (parseAssign p1)
    in
      Option.mapPartial build (parseIdentifier parser)
    end

  fun parseFunc parser =
    case (parseIdentifier parser) of
      NONE => NONE
    | SOME (identifier, p1) =>
        case (parseAssign p1) of
          NONE => NONE
        | SOME p2 =>
            SOME
              (AST.Func {token = Token.Function, identifier = identifier}, p2)


  fun parseProgram parser =
    let
      fun parse currentParser program =
        case (nextToken currentParser) of
          (Token.EOF, _) => SOME program
        | (Token.Let, p) =>
            Option.mapPartial (fn (stmt, p') => parse p' (stmt :: program))
              (parseLet p)
        | (Token.Function, p) =>
            Option.mapPartial (fn (stmt, p') => parse p' (stmt :: program))
              (parseFunc p)
        | _ => SOME program
    in
      Option.map List.rev (parse parser nil)
    end
end
