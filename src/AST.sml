structure AST =
struct

  datatype Expression =
    Identifier of {token: Token.T, value: string}
  | Integer of {token: Token.T, value: int}
  | Boolean of {token: Token.T, value: bool}
  | PrefixExpression of {token: Token.T, operator: string, right: Expression}

  datatype Statement =
    Let of {token: Token.T, identifier: Expression, value: Expression}
  | Func of {token: Token.T, identifier: Expression}
  | If of {token: Token.T, tValue: Expression, fValue: Expression}
  | Return of {token: Token.T, value: Expression}
  | ExpressionStatement of {token: Token.T, value: Expression}

  type Program = Statement list

  fun expToString expression =
    case expression of
      Identifier {token, value} => value
    | Boolean {token, value} => Bool.toString value
    | Integer {token, value} => Int.toString value
    | PrefixExpression {token, operator, right} =>
        String.concat ["(", operator, expToString right, ")"]

  fun toString node =
    case node of
      Let {token, identifier, value} =>
        String.concat
          ["let ", (expToString identifier), " = ", (expToString value), ";\n"]
    | Func {token, identifier} =>
        String.concat [Token.toString token, " ", (expToString identifier)]
    | If {token, ...} => String.concat [Token.toString token, " (todo)"]
    | Return {token, value} =>
        String.concat ["return ", expToString value, ";\n"]
    | ExpressionStatement {token, value} =>
        String.concat [Token.toString token, " ", expToString value]
end
