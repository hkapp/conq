IMAGES = img/dfa/concat.png img/dfa/option.png

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
	rm img/dfa/*.png

conq.pdf: conq.tex dependencies
	$(PDFLATEX) conq.tex > /dev/null

dependencies: $(IMAGES)

img/dfa/%.png: img/dfa/%.dot
	$(DOT) $< -o $@
