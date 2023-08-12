structure Token =
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

fun toString t =
  case t of 
   Illegal s => String.concat ["T:illegal:", s]
  | EOF => ""
  | Ident s => String.concat ["T:ident:", s]
  | Int s => String.concat ["T:int:", s]
  | Assign => "T:="
  | Plus => "T:+"
  | Minus => "T:-"
  | Bang => "T:!"
  | Asterisk => "T:*"
  | Slash => "T:/"
  | LT => "T:<"
  | GT => "T:>"
  | Eq => "T:=="
  | NotEq => "T:!="
  | Comma => "T:,"
  | Semicolon => "T:;"
  | LParen => "T:("
  | RParen => "T:)"
  | LBrace => "T:{"
  | RBrace => "T:}"
  | Function => "T:function"
  | Let => "T:let"
  | True => "T:true"
  | False => "T:false"
  | If => "T:if"
  | Else => "T:else"
  | Return  => "T:return"
end
