structure Parser =
struct
  exception ParseException of string

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
      (Token.Ident i, p) =>
        (AST.Identifier {token = Token.Ident i, value = i}, p)
    | _ => raise ParseException "unable to parse identifier"

  fun parseAssign parser =
    case (nextToken parser) of
      (Token.Assign, p) => p
    | _ => raise ParseException "unable to parse assign"

  fun parseIntegerLiteral parser =
    case (nextToken parser) of
      (Token.Int v, p) =>
        (case (Int.fromString v) of
           SOME i => (AST.Integer {token = Token.Int v, value = i}, p)
         | NONE => raise ParseException "expected a number")
    | _ => raise ParseException "unable to parse integer"

  fun parseOperatorExpression parser =
    let val (token, p1) = nextToken parser
    in raise ParseException "to do"
    end

  fun parseGroupedExpression parser =
    let val (token, p1) = nextToken parser
    in raise ParseException "to do"
    end

  fun parseExpression parser =
    let
      val (currentToken, p1) = nextToken parser
      val peekToken = #peekToken parser
    in
      case (currentToken, peekToken) of
        (Token.Int v, Token.Semicolon) =>
          (case (Int.fromString v) of
             SOME i => (AST.Integer {token = currentToken, value = i}, p1)
           | NONE => raise ParseException "number expected")
      (* or-pattern available only in successorml *)
      | (Token.Int _, Token.Plus) => (parseOperatorExpression parser, p1)
      | (Token.Int _, Token.Minus) => (parseOperatorExpression parser, p1)
      | (Token.Int _, Token.Asterisk) => (parseOperatorExpression parser, p1)
      | (Token.Int _, Token.Slash) => (parseOperatorExpression parser, p1)
      | (Token.True, Token.Semicolon) =>
          (AST.Boolean {token = Token.True, value = true}, p1)
      | (Token.False, Token.Semicolon) =>
          (AST.Boolean {token = Token.False, value = false}, p1)
      | (Token.LParen, _) => (parseGroupedExpression parser, p1)
      | _ => raise ParseException "unable to parse, unknown token"
    end


  fun parseLet parser =
    let
      val (identifier, p1) = parseIdentifier parser
      val p2 = parseAssign p1
      val (value, p3) = parseExpression p2
    in
      (AST.Let {token = Token.Let, identifier = identifier, value = value}, p3)
    end

  fun parseReturn parser =
    case (nextToken parser) of
      (Token.Return, p) =>
        let val (v, p2) = parseExpression p
        in (AST.Return {token = Token.Return, value = v}, p2)
        end
    | _ => raise ParseException "expected return"

  fun parseIf parser =
    let
      val (_, p) = nextToken parser
      val (tv, p2) = parseExpression p
      val (fv, p3) = parseExpression p2
    in
      (AST.If {token = Token.If, tValue = tv, fValue = fv}, p3)
    end

  fun parseFunc parser =
    let
      val (identifier, p1) = parseIdentifier parser
      val p2 = parseAssign p1
    in
      (AST.Func {token = Token.Function, identifier = identifier}, p2)
    end

  fun parseProgram parser =
    let
      fun parse currentParser program =
        case (nextToken currentParser) of
          (Token.EOF, _) => program
        | (Token.Let, p) =>
            let val (stmt, p') = parseLet p
            in parse p' (stmt :: program)
            end
        | (Token.Return, _) =>
            let val (stmt, p') = parseReturn currentParser
            in parse p' (stmt :: program)
            end
        | (Token.If, p) =>
            let val (stmt, p') = parseIf p
            in parse p' (stmt :: program)
            end
        | (Token.Function, p) =>
            let val (stmt, p') = parseFunc p
            in parse p' (stmt :: program)
            end
        | (_, p) => parse p program
    in
      List.rev (parse parser nil)
    end
end
