structure AST =
struct

  datatype Expression =
    Identifier of {token: Token.Token, value: string}
  | Integer of {token: Token.Token, value: int}
  | Boolean of {token: Token.Token, value: bool}
  | Operator of {left: Expression, operator: Token.Token, right: Expression}

  datatype Statement =
    Let of {token: Token.Token, identifier: Expression, value: Expression}
  | Func of {token: Token.Token, identifier: Expression}
  | If of {token: Token.Token, tValue: Expression, fValue: Expression}
  | Return of {token: Token.Token, value: Expression}

  type Program = Statement list

  fun expToString expression =
    case expression of
      Identifier {token, value} =>
        String.concat [Token.toString token, " ", value, " "]
    | Boolean {token, value} =>
        String.concat [Token.toString token, " ", Bool.toString value, " "]
    | Integer {token, value} =>
        String.concat [(Token.toString token), "`", (Int.toString value), "`"]
    | Operator {left, operator, right} =>
        String.concat
          [ (expToString left)
          , " "
          , (Token.toString operator)
          , " "
          , (expToString right)
          ]

  fun toString node =
    case node of
      Let {token, identifier, value} =>
        String.concat
          [ "S: let "
          , (expToString identifier)
          , " <- "
          , (expToString value)
          , ". "
          ]
    | Func {token, identifier} =>
        String.concat [(Token.toString token), " ", (expToString identifier)]
    | If {token, ...} => String.concat [(Token.toString token), " (todo)"]
    | Return {token, value} =>
        String.concat [(Token.toString token), " ", (expToString value)]
end
