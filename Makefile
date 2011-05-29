ALEX_OPTS = --ghc
HAPPY_OPTS = -a -g -c
GHC_OPTS = -dynamic -O2 -fglasgow-exts

all: hemp-parser

hemp-parser: hemp-parser.hs hemp-lexer.hs
	ghc --make $(GHC_OPTS) $^

hemp-lexer.hs: hemp-lexer.x Makefile
	alex $(ALEX_OPTS) $<

hemp-parser.hs: hemp-parser.y Makefile
	happy $(HAPPY_OPTS) $<
