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
      (Token.Ident i, p) => (AST.Identifier {token = token, value = i}, p)
    | _ =>
        raise ParseException
          ("unexpected token while parsing identifier " ^ (Token.toString token))

  and parseIntegerLiteral (token, parser) =
    case token of
      Token.Int v =>
        (case (Int.fromString v) of
           SOME i => (AST.Integer {token = token, value = i}, parser)
         | NONE => raise ParseException "expected a number")
    | _ => raise ParseException "unable to parse integer"

  and parseBooleanLiteral (token, parser) =
    case token of
      Token.True => (AST.Boolean {token = token, value = true}, parser)
    | Token.False => (AST.Boolean {token = token, value = false}, parser)
    | _ =>
        raise ParseException
          ("expected boolean literal, got " ^ (Token.toString token))

  and parseOperatorExpression parser =
    let val (token, p1) = nextToken parser
    in raise ParseException "to do"
    end

  and parseGroupedExpression parser =
    let val (token, p1) = nextToken parser
    in raise ParseException "to do"
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
      val (rightToken, p1) = nextToken parser
      val (rightExpression, p2) = parseExpression Prefix (rightToken, p1) 
    in
      ( AST.PrefixExpression
          { token = token
          , operator = Token.toString token
          , right = rightExpression
          }
      , p2
      )
    end

  and parseInfixExpression leftExp (token, parser)  =
    let
      val precedence = precedences token
      val (rightToken, p1) = nextToken parser
      val (rightExp, p2) = parseExpression precedence (rightToken, p1) 
    in
      ( AST.InfixExpression
          { token = token
          , operator = (Token.toString token)
          , left = leftExp
          , right = rightExp
          }
      , p2
      )
    end

  fun parseAssign parser =
    case (nextToken parser) of
      (Token.Assign, p) => p
    | _ => raise ParseException "unable to parse assign"

  fun parseLet parser =
    let
      val (token, p) = nextToken parser
      val (identifier, p1) = parseIdentifier (token, p)
      val p2 = parseAssign p1
      val (next, p3) = nextToken p2
      val (value, p4) = parseExpression Lowest (next, p3) 
    in
      (AST.Let {token = Token.Let, identifier = identifier, value = value}, p4)
    end

  fun parseReturn parser =
    let
      val (t, p) = nextToken parser
      val (v, p1) = parseExpression Lowest (t, p) 
    in
      (AST.Return {token = Token.Return, value = v}, p1)
    end

  fun parseExpressionStatement token parser =
    let val (expression, p) = parseExpression Lowest (token, parser) 
    in (AST.ExpressionStatement {token = token, value = expression}, p)
    end

  fun parseStatement (token, parser) =
    case token of
      Token.Let => parseLet parser
    | Token.Return => parseReturn parser
    | t => parseExpressionStatement t parser

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
