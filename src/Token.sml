structure Token =
struct
  datatype T =
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

  fun toString t =
    case t of
      Illegal s => "illegal: " ^ s
    | EOF => "<eof>"
    | Ident s => "ident: " ^ s
    | Int s => "int: " ^ s
    | Assign => "="
    | Plus => "+"
    | Minus => "-"
    | Bang => "!"
    | Asterisk => "*"
    | Slash => "/"
    | LT => "<"
    | GT => ">"
    | Eq => "=="
    | NotEq => "!="
    | Comma => ","
    | Semicolon => ";"
    | LParen => "("
    | RParen => ")"
    | LBrace => "{"
    | RBrace => "}"
    | Function => "function"
    | Let => "let"
    | True => "true"
    | False => "false"
    | If => "if"
    | Else => "else"
    | Return => "return"
end
