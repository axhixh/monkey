structure AST =
struct

  datatype Expression =
    Boolean of {token: Token.T, value: bool}
  | Identifier of {token: Token.T, value: string}
  | IfExpression of
      { token: Token.T
      , condition: Expression
      , consequence: Statement
      , alternative: Statement
      }
  | InfixExpression of
      {token: Token.T, left: Expression, operator: string, right: Expression}
  | Integer of {token: Token.T, value: int}
  | PrefixExpression of {token: Token.T, operator: string, right: Expression}
  and Statement =
    BlockStatement of {token: Token.T, statements: Statement list}
  | ExpressionStatement of {token: Token.T, value: Expression}
  | Func of {token: Token.T, identifier: Expression}
  | Let of {token: Token.T, identifier: Expression, value: Expression}
  | Return of {token: Token.T, value: Expression}

  type Program = Statement list

  fun expToString expression =
    case expression of
      Boolean {token, value} => Bool.toString value
    | Identifier {token, value} => value
    | IfExpression {token, condition, consequence, alternative} =>
        "if " ^ (expToString condition) ^ (toString consequence)
        ^ (toString alternative)
    | InfixExpression {token, left, operator, right} =>
        "(" ^ (expToString left) ^ " " ^ operator ^ " " ^ (expToString right)
        ^ ")"
    | Integer {token, value} => Int.toString value
    | PrefixExpression {token, operator, right} =>
        "(" ^ operator ^ (expToString right) ^ ")"

  and toString node =
    let
      fun statementsToString statements =
        String.concatWith ";\n" (List.map toString statements)
    in
      case node of
        BlockStatement {token, statements} =>
          "{\n" ^ (statementsToString statements) ^ "}\n"
      | ExpressionStatement {token, value} => (expToString value)
      | Func {token, identifier} =>
          (Token.toString token) ^ " " ^ (expToString identifier)
      | Let {token, identifier, value} =>
          "let " ^ (expToString identifier) ^ " = " ^ (expToString value)
          ^ ";\n"
      | Return {token, value} => "return " ^ (expToString value) ^ ";\n"
    end
end
