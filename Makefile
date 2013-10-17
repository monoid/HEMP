ALEX_OPTS = --ghc
HAPPY_OPTS = -a -g -c
GHC_OPTS = -O2 -XGADTs

all: HempParser

HempParser: HempDecl.hs HempParser.hs HempLexer.hs HempTypes.hs
	ghc --make $(GHC_OPTS) $^

HempLexer.hs: HempLexer.x Makefile
	alex $(ALEX_OPTS) $<

HempParser.hs: HempParser.y Makefile
	happy $(HAPPY_OPTS) $<
