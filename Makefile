all: hemp-lexer

hemp-lexer: hemp-lexer.hs
	ghc --make $<

hemp-lexer.hs: hemp-lexer.x
	alex $<
