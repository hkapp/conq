
bin = ../bin
conq = ../bin/conq
GHC = ghc -outputdir $(bin)

all: conq

clean:
	rm $(bin)/*

test: conq
	$(conq) --test
	
run: conq
	$(conq) --generate abc

conq:
	$(GHC) -o $(conq) Main.hs
