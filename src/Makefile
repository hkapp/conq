
bin = ../bin
conq = ../bin/conq
GHC = ghc -outputdir $(bin)

all: conq

clean:
	rm $(bin)/*

test: conq
	$(conq)

conq:
	$(GHC) -o $(conq) Main.hs
