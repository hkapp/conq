
bin = ../bin
conq = ../bin/conq
gendir = ../gen
GHC = ghc -outputdir $(bin)

all: conq

clean:
	rm $(bin)/*

test: conq
	$(conq) --print

run: conq $(gendir)
	$(conq) --generate abc

$(gendir):
	mkdir $(gendir)

conq:
	$(GHC) -o $(conq) Main.hs
