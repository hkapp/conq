
bin = ../bin
conq = ../bin/conq
gendir = ../gen
GHC = ghc -outputdir $(bin)

all: conq

clean:
	rm $(bin)/*

test: conq
	$(conq) --test

run: conq $(gendir)
	$(conq) --generate abc

$(gendir):
	mkdir $(gendir)

$(bin):
	mkdir $(bin)

conq: $(bin)
	$(GHC) -o $(conq) Main.hs
