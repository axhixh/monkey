clean:
	rm -f src/*.ui
	rm -f src/*.uo
	rm -rf bin

Token.uo: src/Token.sml
	mosmlc -c $<

Lexer.uo: src/Lexer.sml Token.uo
	mosmlc -I src -c $< 

Parser.uo: src/Parser.sml Lexer.uo Token.uo
	mosmlc -I src -c $<

repl: src/Repl.sml Token.uo Lexer.uo Parser.uo
	mkdir -p bin
	mosmlc -standalone -o bin/$@ -I src $< 
