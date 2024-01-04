clean:
	rm -f src/*.ui
	rm -f src/*.uo
	rm -rf bin

repl: src/*.sml
	mkdir -p bin
	mosmlc -standalone -o bin/$@ src/Token.sml src/Lexer.sml src/Parser.sml src/Repl.sml
