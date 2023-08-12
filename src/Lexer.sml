structure Lexer =
struct
  datatype Token =
    Illegal of string
  | EOF
  (* Identifiers and literals *)
  | Ident of string
  | Int of string
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

  fun isLetter ch =
    #"a" <= ch andalso #"z" >= ch orelse #"A" <= ch andalso #"Z" >= ch
    orelse #"_" = ch

  fun scanTill input index untilFn =
    if untilFn (String.sub (input, index)) then
      scanTill input (index + 1) untilFn
    else
      index

  fun readIdentifier (l: LexerT) : string * LexerT =
    let
      val s = #position l
      val e = scanTill (#input l) s isLetter
    in
      ( String.substring (#input l, s, (e - s))
      , { input = #input l
        , position = e - 1
        , readPosition = e
        , ch = String.sub (#input l, e)
        }
      )
    end

  fun lookupIdent ident =
    case ident of
      "fn" => Function
    | "let" => Let
    | "true" => True
    | "false" => False
    | "if" => If
    | "else" => Else
    | "return" => Return
    | other => Ident other

  fun skipWhitespace (l: LexerT) : LexerT =
    let
      val ch = #ch l
    in
      if ch = #" " orelse ch = #"\t" orelse ch = #"\n" orelse ch = #"\r" then
        skipWhitespace (readChar l)
      else
        l
    end

  fun isDigit ch = #"0" <= ch andalso #"9" >= ch

  fun readNumber (l: LexerT) : string * LexerT =
    let
      val s = #position l
      val e = scanTill (#input l) (#position l) isDigit
    in
      ( String.substring (#input l, s, (e - s))
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
      val ch = #ch l
    in
      let
        val (token, l') =
          case ch of
            #";" => (Semicolon, l)
          | #"(" => (LParen, l)
          | #")" => (RParen, l)
          | #"," => (Comma, l)
          | #"+" => (Plus, l)
          | #"-" => (Minus, l)
          | #"*" => (Asterisk, l)
          | #"/" => (Slash, l)
          | #"<" => (LT, l)
          | #">" => (GT, l)
          | #"{" => (LBrace, l)
          | #"}" => (RBrace, l)
          | #"=" => if peekChar l = #"=" then (Eq, readChar l) else (Assign, l)
          | #"!" => if peekChar l = #"=" then (NotEq, readChar l) else (Bang, l)
          | ch =>
              if ch = Char.minChar then
                (EOF, l)
              else if isLetter ch then
                let val (ident, l2) = readIdentifier l
                in (lookupIdent ident, l2)
                end
              else if isDigit ch then
                let val (number, l2) = readNumber l
                in (Int number, l2)
                end
              else
                (Illegal (Char.toString ch), l)
      in
        (token, readChar l')
      end
    end
end
