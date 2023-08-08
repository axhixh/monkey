structure Lexer =
struct
  datatype TokenType =
    Illegal
  | EOF
  (* Identifiers and literals *)
  | Ident
  | Int
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LT
  | GT
  | Eq
  | NotEq
  (* Delimiters *)
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  (* Keywords *)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return

  type Token = {tokenType: TokenType, literal: string}

  type LexerT = {input: string, position: int, readPosition: int, ch: char}

  fun readChar (l: LexerT) : LexerT =
    if #readPosition l >= String.size (#input l) then
      { input = #input l
      , position = #position l
      , readPosition = #readPosition l
      , ch = Char.minChar
      }
    else
      { input = #input l
      , position = #readPosition l
      , readPosition = #readPosition l + 1
      , ch = String.sub (#input l, #readPosition l)
      }

  fun peekChar (l: LexerT) : char =
    if #readPosition l >= String.size (#input l) then Char.minChar
    else String.sub (#input l, #readPosition l)

  fun isLetter (ch: char) : bool =
    #"a" <= ch andalso #"z" >= ch orelse #"A" <= ch andalso #"Z" >= ch
    orelse #"_" = ch

  fun scanIdent input index =
    if isLetter (String.sub (input, index)) then scanIdent input (index + 1)
    else index

  fun readIdentifier (l: LexerT) : string * LexerT =
    let
      val s = #position l
      val e = scanIdent (#input l) (#position l)
    in
      ( String.substring (#input l, s, e)
      , { input = #input l
        , position = e - 1
        , readPosition = e
        , ch = String.sub (#input l, e)
        }
      )
    end

  fun lookupIdent (ident: string) : TokenType =
    case ident of
      "fn" => Function
    | "let" => Let
    | "true" => True
    | "false" => False
    | "if" => If
    | "else" => Else
    | "return" => Return
    | x => Ident

  fun skipWhitespace (l: LexerT) : LexerT =
    let
      val ch = #ch l
    in
      if ch = #" " orelse ch = #"\t" orelse ch = #"\n" orelse ch = #"\r" then
        skipWhitespace (readChar l)
      else
        l
    end

  fun isDigit (ch: char) : bool = #"0" <= ch andalso #"9" >= ch

  fun scanNumber input index =
    if isDigit (String.sub (input, index)) then scanNumber input (index + 1)
    else index

  fun readNumber (l: LexerT) : string * LexerT =
    let
      val s = #position l
      val e = scanNumber (#input l) (#position l)
    in
      ( String.substring (#input l, s, e)
      , { input = #input l
        , position = e - 1
        , readPosition = e
        , ch = String.sub (#input l, e)
        }
      )
    end

  fun new code =
    readChar ({input = code, position = 0, readPosition = 0, ch = Char.minChar})

  fun nextToken (l: LexerT) : Token * LexerT =
    let
      val l = skipWhitespace l
    in
      let
        val ch = #ch l
        val (token, l') =
          if ch = #"=" then
            ({tokenType = Assign, literal = Char.toString (ch)}, l)
          else if ch = #";" then
            ({tokenType = Semicolon, literal = Char.toString (ch)}, l)
          else if ch = #"(" then
            ({tokenType = LParen, literal = Char.toString (ch)}, l)
          else if ch = #")" then
            ({tokenType = RParen, literal = Char.toString (ch)}, l)
          else if ch = #"," then
            ({tokenType = Comma, literal = Char.toString (ch)}, l)
          else if ch = #"+" then
            ({tokenType = Plus, literal = Char.toString (ch)}, l)
          else if ch = #"-" then
            ({tokenType = Minus, literal = Char.toString (ch)}, l)
          else if ch = #"*" then
            ({tokenType = Asterisk, literal = Char.toString (ch)}, l)
          else if ch = #"/" then
            ({tokenType = Slash, literal = Char.toString (ch)}, l)
          else if ch = #"!" then
            ({tokenType = Bang, literal = Char.toString (ch)}, l)
          else if ch = #"<" then
            ({tokenType = LT, literal = Char.toString (ch)}, l)
          else if ch = #">" then
            ({tokenType = GT, literal = Char.toString (ch)}, l)
          else if ch = #"{" then
            ({tokenType = LBrace, literal = Char.toString (ch)}, l)
          else if ch = #"}" then
            ({tokenType = RBrace, literal = Char.toString (ch)}, l)
          else if ch = #"=" then
            if peekChar l = #"=" then
              ({tokenType = Eq, literal = "=="}, readChar l)
            else
              ({tokenType = Assign, literal = Char.toString (ch)}, l)
          else if ch = #"!" then
            if peekChar l = #"=" then
              ({tokenType = NotEq, literal = "!="}, readChar l)
            else
              ({tokenType = Bang, literal = Char.toString (ch)}, l)
          else if ch = Char.minChar then
            ({tokenType = EOF, literal = Char.toString (ch)}, l)
          else if isLetter (ch) then
            let val (ident, l2) = readIdentifier (l)
            in ({tokenType = lookupIdent ident, literal = ident}, l2)
            end
          else if isDigit (ch) then
            let val (number, l2) = readNumber (l)
            in ({tokenType = Int, literal = number}, l2)
            end
          else
            ({tokenType = Illegal, literal = Char.toString (ch)}, l)
      in
        (token, readChar (l'))
      end
    end
end
