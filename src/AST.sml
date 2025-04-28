structure AST =
struct

  (* The book retains the token used to parse the expression and statements.
   * When writing and debugging code, I never used the token so removing it
   * to make it simpler.
   *)
  datatype Expression =
    Boolean of bool
  | CallExpression of {function: Expression, arguments: Expression list}
  | Func of {parameters: Expression list, body: Statement}
  | Identifier of string
  | IfExpression of
      { condition: Expression
      , consequence: Statement
      , alternative: Statement option
      }
  | InfixExpression of {left: Expression, operator: string, right: Expression}
  | Integer of int
  | PrefixExpression of {operator: string, right: Expression}
  and Statement =
    BlockStatement of Statement list
  | ExpressionStatement of Expression
  | Let of {identifier: Expression, value: Expression}
  | Return of Expression

  type Program = Statement list

  fun expToString expression =
    case expression of
      Boolean value => Bool.toString value
    | CallExpression {function, arguments} =>
        (expToString function) ^ "("
        ^
        (List.foldl
           (fn (s, a) =>
              a ^ (if String.size a = 0 then "" else ", ") ^ (expToString s)) ""
           arguments) ^ ")"
    | Func {parameters, body} =>
        "fn ("
        ^
        (List.foldl
           (fn (s, a) =>
              a ^ (if String.size a = 0 then "" else ", ") ^ (expToString s)) ""
           parameters) ^ ") " ^ (toString body)
    | Identifier value => value
    | IfExpression {condition, consequence, alternative} =>
        "if " ^ (expToString condition) ^ (toString consequence)
        ^
        (if Option.isSome alternative then
           ("else " ^ toString (Option.valOf alternative))
         else
           "")
    | InfixExpression {left, operator, right} =>
        "(" ^ (expToString left) ^ " " ^ operator ^ " " ^ (expToString right)
        ^ ")"
    | Integer value => Int.toString value
    | PrefixExpression {operator, right} =>
        "(" ^ operator ^ (expToString right) ^ ")"

  and toString node =
    let
      fun statementsToString statements =
        String.concatWith ";\n" (List.map toString statements)
    in
      case node of
        BlockStatement statements =>
          "{\n" ^ (statementsToString statements) ^ "\n}\n"
      | ExpressionStatement value => (expToString value)
      | Let {identifier, value} =>
          "let " ^ (expToString identifier) ^ " = " ^ (expToString value)
          ^ ";\n"
      | Return value => "return " ^ (expToString value) ^ ";\n"
    end
end
