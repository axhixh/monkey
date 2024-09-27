structure Lexer =
struct
  type LexerT = {input: string, position: int, readPosition: int, ch: char}

  fun readChar (lexer: LexerT) =
    if #readPosition lexer >= String.size (#input lexer) then
      { input = #input lexer
      , position = #position lexer
      , readPosition = #readPosition lexer
      , ch = Char.minChar
      }
    else
      { input = #input lexer
      , position = #readPosition lexer
      , readPosition = #readPosition lexer + 1
      , ch = String.sub (#input lexer, #readPosition lexer)
      }

  fun peekChar (lexer: LexerT) =
    if #readPosition lexer >= String.size (#input lexer) then Char.minChar
    else String.sub (#input lexer, #readPosition lexer)

  fun isLetter ch =
    #"a" <= ch andalso #"z" >= ch orelse #"A" <= ch andalso #"Z" >= ch
    orelse #"_" = ch

  fun scanTill input index untilFn =
    if untilFn (String.sub (input, index)) then
      scanTill input (index + 1) untilFn
    else
      index

  fun readIdentifier (lexer: LexerT) =
    let
      val s = #position lexer
      val e = scanTill (#input lexer) s isLetter
    in
      ( String.substring (#input lexer, s, (e - s))
      , { input = #input lexer
        , position = e - 1
        , readPosition = e
        , ch = String.sub (#input lexer, e)
        }
      )
    end

  fun lookupIdent ident =
    case ident of
      "fn" => Token.Function
    | "let" => Token.Let
    | "true" => Token.True
    | "false" => Token.False
    | "if" => Token.If
    | "else" => Token.Else
    | "return" => Token.Return
    | other => Token.Ident other

  fun skipWhitespace (lexer: LexerT) =
    let
      val ch = #ch lexer
    in
      if ch = #" " orelse ch = #"\t" orelse ch = #"\n" orelse ch = #"\r" then
        skipWhitespace (readChar lexer)
      else
        lexer
    end

  fun isDigit ch = #"0" <= ch andalso #"9" >= ch

  fun readNumber (lexer: LexerT) =
    let
      val s = #position lexer
      val e = scanTill (#input lexer) (#position lexer) isDigit
    in
      ( String.substring (#input lexer, s, (e - s))
      , { input = #input lexer
        , position = e - 1
        , readPosition = e
        , ch = String.sub (#input lexer, e)
        }
      )
    end

  fun new code =
    readChar {input = code, position = 0, readPosition = 0, ch = Char.minChar}

  fun nextToken (lexer: LexerT) =
    let
      val l = skipWhitespace lexer
      val ch = #ch l
    in
      let
        val (token, l') =
          case ch of
            #";" => (Token.Semicolon, l)
          | #"(" => (Token.LParen, l)
          | #")" => (Token.RParen, l)
          | #"," => (Token.Comma, l)
          | #"+" => (Token.Plus, l)
          | #"-" => (Token.Minus, l)
          | #"*" => (Token.Asterisk, l)
          | #"/" => (Token.Slash, l)
          | #"<" => (Token.LT, l)
          | #">" => (Token.GT, l)
          | #"{" => (Token.LBrace, l)
          | #"}" => (Token.RBrace, l)
          | #"=" =>
              if peekChar l = #"=" then (Token.Eq, readChar l)
              else (Token.Assign, l)
          | #"!" =>
              if peekChar l = #"=" then (Token.NotEq, readChar l)
              else (Token.Bang, l)
          | _ =>
              if ch = Char.minChar then
                (Token.EOF, l)
              else if isLetter ch then
                let val (ident, l2) = readIdentifier l
                in (lookupIdent ident, l2)
                end
              else if isDigit ch then
                let val (number, l2) = readNumber l
                in (Token.Int number, l2)
                end
              else
                (Token.Illegal (Char.toString ch), l)
      in
        (token, readChar l')
      end
    end
end
