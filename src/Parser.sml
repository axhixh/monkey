structure Parser =
struct
  exception ParseException of string

  type ParserT = {lexer: Lexer.LexerT, currToken: Token.T, peekToken: Token.T}

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

  datatype Precedence =
    Lowest
  | Equals
  | LessGreater
  | Sum
  | Product
  | Prefix
  | Call

  fun precedences token =
    case token of
      Token.Eq => Equals
    | Token.NotEq => Equals
    | Token.LT => LessGreater
    | Token.GT => LessGreater
    | Token.Plus => Sum
    | Token.Minus => Sum
    | Token.Slash => Product
    | Token.Asterisk => Product
    | _ => Lowest

  fun compare p1 p2 =
    let
      fun ordinal p =
        case p of
          Lowest => 1
        | Equals => 2
        | LessGreater => 3
        | Sum => 4
        | Product => 5
        | Prefix => 6
        | Call => 7
      val o1 = ordinal p1
      val o2 = ordinal p2
    in
      o1 - o2
    end

  fun parseIdentifier token parser =
    case (token, parser) of
      (Token.Ident i, p) => (AST.Identifier {token = token, value = i}, p)
    | _ =>
        raise ParseException
          (String.concat
             [ "unexpected token while parsing identifier "
             , Token.toString token
             ])

  fun parseAssign parser =
    case (nextToken parser) of
      (Token.Assign, p) => p
    | _ => raise ParseException "unable to parse assign"

  fun parseIntegerLiteral token parser =
    case token of
      Token.Int v =>
        (case (Int.fromString v) of
           SOME i => (AST.Integer {token = token, value = i}, parser)
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
        (Token.Int _, Token.Semicolon) => parseIntegerLiteral currentToken p1
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

  (* the book uses a map parse functions for each token,
   * methods to register them. we are going to use lookup
   * functions and code the parse functions to token using
   * case statement
   *)
  fun prefixParseFn token =
    case token of
      Token.Ident _ => parseIdentifier
    | Token.Int _ => parseIntegerLiteral
    | _ => raise ParseException "unknown token for prefix parse function"

  fun infixParseFn token =
    case token of
      Token.Plus => raise ParseException "not implementd"
    | Token.Minus => raise ParseException "not implemented"
    | _ => raise ParseException "unknown token for infix parse function"

  fun parseLet parser =
    let
      val (token, p) = nextToken parser
      val (identifier, p1) = parseIdentifier token p
      val p2 = parseAssign p1
      val (value, p3) = parseExpression p2
    in
      (AST.Let {token = Token.Let, identifier = identifier, value = value}, p3)
    end

  fun parseReturn parser =
    let val (v, p) = parseExpression parser
    in (AST.Return {token = Token.Return, value = v}, p)
    end

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
      val (token, p) = nextToken parser
      val (identifier, p1) = parseIdentifier token p
      val p2 = parseAssign p1
    in
      (AST.Func {token = Token.Function, identifier = identifier}, p2)
    end

  fun parseExpressionStatement token parser =
    let
      val prefixFn = prefixParseFn token
      val (prefix, p) = prefixFn token parser
    in
      (AST.ExpressionStatement {token = token, value = prefix}, p)
    end

  fun parseStatement token parser =
    case token of
      Token.Let => parseLet parser
    | Token.Return => parseReturn parser
    | Token.If => parseIf parser
    | Token.Function => parseFunc parser
    | t => parseExpressionStatement t parser

  fun parseProgram parser =
    let
      fun parse currentParser program =
        case (nextToken currentParser) of
          (Token.EOF, _) => program
        | (Token.Semicolon, p) => parse p program
        | (t, p) =>
            let val (stmt, p') = parseStatement t p
            in parse p' (stmt :: program)
            end

    in
      List.rev (parse parser nil)
    end
end
