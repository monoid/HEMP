ALEX_OPTS = --ghc
GHC_OPTS = -dynamic -O2

all: hemp-lexer

hemp-lexer: hemp-lexer.hs
	ghc --make $(GHC_OPTS) $<

hemp-lexer.hs: hemp-lexer.x
	alex $(ALEX_OPTS) $<
