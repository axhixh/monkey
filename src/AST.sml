structure AST =
struct

  datatype Expression =
    Boolean of {token: Token.T, value: bool}
  | Identifier of {token: Token.T, value: string}
  | InfixExpression of
      {token: Token.T, left: Expression, operator: string, right: Expression}
  | Integer of {token: Token.T, value: int}
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
      Boolean {token, value} => Bool.toString value
    | Identifier {token, value} => value
    | InfixExpression {token, left, operator, right} =>
        "(" ^ (expToString left) ^ " " ^ operator ^ " " ^ (expToString right)
        ^ ")"
    | Integer {token, value} => Int.toString value
    | PrefixExpression {token, operator, right} =>
        "(" ^ operator ^ (expToString right) ^ ")"

  fun toString node =
    case node of
      Let {token, identifier, value} =>
        "let " ^ (expToString identifier) ^ " = " ^ (expToString value) ^ ";\n"
    | Func {token, identifier} =>
        (Token.toString token) ^ " " ^ (expToString identifier)
    | If {token, ...} => (Token.toString token) ^ " (todo)"
    | Return {token, value} => "return " ^ (expToString value) ^ ";\n"
    | ExpressionStatement {token, value} =>
        (Token.toString token) ^ " " ^ (expToString value)
end
