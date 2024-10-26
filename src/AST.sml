structure AST =
struct

  type Identifier = {ident: Token.Token, value: string}

  type Expression = {identifier: string, value: string}

  datatype Statement =
    Let of {token: Token.Token, identifier: Identifier, value: Expression}
  | Func of {token: Token.Token, identifier: Identifier}
  | If of {token: Token.Token, tValue: Expression, fValue: Expression}
  | Return of {token: Token.Token, value: Expression}

  type Program = Statement list

  fun idToString {ident, value} = String.concat ["ident(", value, ")"]

  fun expToString {identifier, value} =
    String.concat [identifier, "`", value, "`"]

  fun toString node =
    case node of
      Let {token, identifier, value} =>
        String.concat
          [ "S: let "
          , (idToString identifier)
          , " <- "
          , (expToString value)
          , ". "
          ]
    | Func {token, identifier} =>
        String.concat [(Token.toString token), " ", (idToString identifier)]
    | If {token, ...} => String.concat [(Token.toString token), " (todo)"]
    | Return {token, value} =>
        String.concat [(Token.toString token), " ", (expToString value)]
end
