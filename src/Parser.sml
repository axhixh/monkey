structure Parser =
struct

  datatype Statement =
    LetStatement of {token: Token.Token, identifier: string, value: Expression}
  | FuncStatement

  type Expression = {identifier: string, value: string}
   
  datatype Node = Statement of Statement | Expression of Expression
  type Program = Statement list

  type ParserT =
    {lexer: Lexer.LexerT, currToken: Token.Token, peekToken: Token.Token}

  fun new lexer =
    let
      val (current, lexer) = Lexer.nextToken lexer
      val (peek, lexer) = Lexer.nextToken lexer
    in
      {lexer = lexer, currTokene = current, peekToken = peek}
    end

  fun nextToken parser =
    let
      val current = #currToken parser
      val peek = #peekToken parser
      val (next, lexer) = Lexer.nextToken (#lexer parser)
    in
      (current, {lexer = lexer, currToken = peek, peekToken = next})
    end
end
