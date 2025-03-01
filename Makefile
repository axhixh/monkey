clean:
	rm -f src/*.ui
	rm -f src/*.uo
	rm -rf bin

AssocList.uo: src/AssocList.sml
	mosmlc -c $<

AssocListTest: src/AssocListTest.sml AssocList.uo
	mkdir -p bin
	mosmlc -standalone -o bin/assoc-list-test -I src $< 

Util.uo: src/Util.sml
	mosmlc -c $<

Token.uo: src/Token.sml
	mosmlc -c $<

Lexer.uo: src/Lexer.sml Token.uo
	mosmlc -I src -c $< 

AST.uo: src/AST.sml Token.uo
	mosmlc -I src -c $<

Parser.uo: src/Parser.sml AST.uo Lexer.uo Util.uo AssocList.uo
	mosmlc -I src -c $<

ParserTest: src/ParserTest.sml Parser.uo
	mkdir -p bin
	mosmlc -standalone -o bin/parser-test -I src $<
	
repl: src/Repl.sml Parser.uo 
	mkdir -p bin
	mosmlc -standalone -o bin/$@ -I src $< 
