main: generar
	runhaskell main.hs
generar: clean 
	alex Tokens.x -o Tokens.hs
	happy Grammar.y -o Grammar.hs
clean:
	rm -f Tokens.hs
	rm -f Grammar.hs
