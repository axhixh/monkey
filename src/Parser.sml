structure Parser =
struct
  exception ParseException of string

  (* 
  * the book has peekToken, but since parser is immutable in our 
  * implementation, we can skip it since we can have two different
  * instance of parser representing the differennt states
  *)
  type T = {lexer: Lexer.LexerT, currToken: Token.T}

  fun new lexer =
    let val (current, lexer) = Lexer.nextToken lexer
    in {lexer = lexer, currToken = current}
    end

  fun nextToken (parser: T) =
    let
      val current = #currToken parser
      val (next, lexer) = Lexer.nextToken (#lexer parser)
    in
      (current, {lexer = lexer, currToken = next})
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

  (* the book uses a map parse functions for each token,
   * methods to register them. we are going to use lookup
   * functions and code the parse functions to token using
   * case statement
   *)
  fun prefixParseFn token =
    case token of
      Token.Ident _ => SOME parseIdentifier
    | Token.Int _ => SOME parseIntegerLiteral
    | Token.False => SOME parseBooleanLiteral
    | Token.True => SOME parseBooleanLiteral
    | Token.Bang => SOME parsePrefixExpression
    | Token.Minus => SOME parsePrefixExpression
    | Token.LParen => SOME parseGroupedExpression
    | Token.If => SOME parseIfExpression
    | Token.Function => SOME parseFunctionLiteral
    | _ => NONE

  and infixParseFn token =
    case token of
      Token.Plus => SOME parseInfixExpression
    | Token.Minus => SOME parseInfixExpression
    | Token.Slash => SOME parseInfixExpression
    | Token.Asterisk => SOME parseInfixExpression
    | Token.Eq => SOME parseInfixExpression
    | Token.NotEq => SOME parseInfixExpression
    | Token.LT => SOME parseInfixExpression
    | Token.GT => SOME parseInfixExpression
    | _ => NONE

  and parseIdentifier (token, parser) =
    case (token, parser) of
      (Token.Ident i, p) => (AST.Identifier i, p)
    | _ =>
        raise ParseException
          ("unexpected token while parsing identifier " ^ (Token.toString token))

  and parseIntegerLiteral (token, parser) =
    case token of
      Token.Int v =>
        (case (Int.fromString v) of
           SOME i => (AST.Integer i, parser)
         | NONE => raise ParseException "expected a number")
    | _ => raise ParseException "unable to parse integer"

  and parseBooleanLiteral (token, parser) =
    case token of
      Token.True => (AST.Boolean true, parser)
    | Token.False => (AST.Boolean false, parser)
    | _ =>
        raise ParseException
          ("expected boolean literal, got " ^ (Token.toString token))

  and parseGroupedExpression (token, parser) =
    let
      val (exp, p1) = parseExpression Lowest (nextToken parser)
      val (t2, p2) = nextToken p1
    in
      if Token.RParen = t2 then (exp, p2)
      else raise ParseException ("expected ) but got " ^ (Token.toString t2))
    end

  and parseIfExpression (token, parser) =
    let
      val (t1, p1) = nextToken parser
      val (condition, p2) =
        if Token.LParen = t1 then
          parseExpression Lowest (t1, p1)
        else
          raise ParseException
            ("expected ( for if expression, but got " ^ (Token.toString t1))
      val (consequence, p3) = parseBlockStatement p2
      val (alternative, p4) =
        case (nextToken p3) of
          (Token.Else, p5) =>
            let val (stmt, p) = parseBlockStatement p5
            in (SOME stmt, p)
            end
        | _ => (NONE, p3)
    in
      ( AST.IfExpression
          { condition = condition
          , consequence = consequence
          , alternative = alternative
          }
      , p4
      )
    end

  and parseExpression precedence (token, parser) =
    let
      fun loop lExp lPrecedence lParser =
        let
          val (t1, p1) = nextToken lParser
          val precedenceDiff = compare lPrecedence (precedences t1)
          val parseMore = Token.Semicolon <> t1 andalso precedenceDiff < 0
        in
          if parseMore then
            case (infixParseFn t1) of
              NONE => (lExp, lParser)
            | SOME infixFn =>
                let val (lExp2, p2) = infixFn lExp (t1, p1)
                in loop lExp2 lPrecedence p2
                end
          else
            (lExp, lParser)
        end
    in
      case (prefixParseFn token) of
        NONE =>
          raise ParseException
            ("didn't find prefix parse function for " ^ (Token.toString token))
      | SOME prefixFn =>
          let val (leftExp, p1: T) = prefixFn (token, parser)
          in loop leftExp precedence p1
          end
    end

  and parsePrefixExpression (token, parser) =
    let
      val (rightExpression, p1) = parseExpression Prefix (nextToken parser)
    in
      ( AST.PrefixExpression
          {operator = Token.toString token, right = rightExpression}
      , p1
      )
    end

  and parseInfixExpression leftExp (token, parser) =
    let
      val (rightExp, p) = parseExpression (precedences token) (nextToken parser)
    in
      ( AST.InfixExpression
          {operator = (Token.toString token), left = leftExp, right = rightExp}
      , p
      )
    end

  and parseBlockStatement parser =
    let
      fun parse currentParser statements =
        case (nextToken currentParser) of
          (Token.EOF, p) => (statements, p)
        | (Token.RBrace, p) => (statements, p)
        | (Token.Semicolon, p) => parse p statements
        | (t, p) =>
            let val (stmt, p') = parseStatement (t, p)
            in parse p' (stmt :: statements)
            end
    in
      let
        val (t, p') = nextToken parser
        val (stmts, p) = parse p' nil
      in
        (AST.BlockStatement (List.rev stmts), p)
      end
    end

  and parseAssign parser =
    case (nextToken parser) of
      (Token.Assign, p) => p
    | _ => raise ParseException "unable to parse assign"

  and parseFunctionParameters parser =
    let
      fun parse currentParser identifiers =
        case (nextToken currentParser) of
          (Token.EOF, p) => (identifiers, p)
        | (Token.RParen, p) => (identifiers, p)
        | (Token.Comma, p) => parse p identifiers
        | (t, p) =>
            let val (identifier, p') = parseIdentifier (t, p)
            in parse p' (identifier :: identifiers)
            end
      val (params, p) = parse parser nil
    in
      (List.rev params, p)
    end

  and parseFunctionLiteral (token, parser) =
    let
      val (t, p) = nextToken parser
      val (parameters, p1) =
        if t = Token.LParen then parseFunctionParameters p
        else raise ParseException "error parsing function expected ("
      val (body, p2) = parseBlockStatement p1
    in
      (AST.Func {parameters = parameters, body = body}, p2)
    end

  and parseLet parser =
    let
      val (identifier, p1) = parseIdentifier (nextToken parser)
      val p2 = parseAssign p1
      val (value, p3) = parseExpression Lowest (nextToken p2)
    in
      (AST.Let {identifier = identifier, value = value}, p3)
    end

  and parseReturn parser =
    let val (v, p1) = parseExpression Lowest (nextToken parser)
    in (AST.Return v, p1)
    end

  and parseExpressionStatement (token, parser) =
    let val (expression, p) = parseExpression Lowest (token, parser)
    in (AST.ExpressionStatement expression, p)
    end

  and parseStatement (token, parser) =
    case token of
      Token.Let => parseLet parser
    | Token.Return => parseReturn parser
    | t => parseExpressionStatement (t, parser)

  fun parseProgram parser =
    let
      fun parse currentParser program =
        case (nextToken currentParser) of
          (Token.EOF, _) => program
        | (Token.Semicolon, p) => parse p program
        | (t, p) =>
            let val (stmt, p') = parseStatement (t, p)
            in parse p' (stmt :: program)
            end
    in
      List.rev (parse parser nil)
    end
end
