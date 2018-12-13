
bin = ../bin
GHC = ghc -outputdir $(bin)

all: main

clean:
	rm $(bin)/*
	
test: main
	$(bin)/main
	
main: $(bin)/main

$(bin)/main: main.hs
	$(GHC) -o $(bin)/main main.hs
