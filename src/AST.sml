structure AST =
struct

  type Identifier = {ident: Token.Token, value: string}

  type Expression = {identifier: string, value: string}

  datatype Statement =
    Let of {token: Token.Token, identifier: Identifier, value: Expression}
  | Func of {token: Token.Token, identifier: Identifier}

  type Program = Statement list

  fun idToString {ident, value} =
    String.concat [(Token.toString ident), " v:", value]

  fun expToString {identifier, value} = String.concat [identifier, " v:", value]

  fun toString node =
    case node of
      Let {token, identifier, value} =>
        String.concat
          [ "S -> "
          , (Token.toString token)
          , " "
          , (idToString identifier)
          , " "
          , (expToString value)
          ]
    | Func {token, identifier} =>
        String.concat [(Token.toString token), " ", (idToString identifier)]
end