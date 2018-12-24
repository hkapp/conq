NFADIR = img/nfa
DFADIR = img/dfa
IRDIR = img/ir

NFAPNG = $(shell ls -1 $(NFADIR)/*.dot | sed s/dot/png/)
DFAPNG = $(shell ls -1 $(DFADIR)/*.dot | sed s/dot/png/)
IRPNG = $(shell ls -1 $(IRDIR)/*.dot | sed s/dot/png/)
PNG = $(NFAPNG) $(DFAPNG) $(IRPNG)

PDFLATEX=pdflatex -synctex=1
DOT=dot -Tpng

# When doing dynamic rules :
# % can only be used in the product and the requirements
# In the command, $* gives the value of %
# $< gives the list of requirements
# $@ gives the full name of the target
# In $< and $@, % gets replaced by its instantiation value

all: conq.pdf

clean:
	rm *.aux *.log *.out *.pdf *.gz *.toc
	rm $(NFADIR)/*.png $(DFADIR)/*.png $(IRDIR)/*.png

conq.pdf: conq.tex $(PNG)
	$(PDFLATEX) conq.tex > /dev/null
	$(PDFLATEX) conq.tex > /dev/null

dependencies: $(PNG)

img/nfa/%.png: img/nfa/%.dot
	$(DOT) $< -o $@

img/dfa/%.png: img/dfa/%.dot
	$(DOT) $< -o $@

img/ir/%.png: img/ir/%.dot
	$(DOT) $< -o $@
