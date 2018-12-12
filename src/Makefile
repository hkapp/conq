
bin = ../bin
GHC = ghc -outputdir $(bin)

all: main

clean:
	rm $(bin)/*

main: main.hs
	$(GHC) -o $(bin)/main main.hs
