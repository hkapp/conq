IMAGES = img/dfa/concat.png img/dfa/option.png img/dfa/kleene-star.png img/dfa/repeat-at-least-once.png img/dfa/repeat-n-times.png img/dfa/repeat-at-least-n-times.png img/dfa/repeat-at-most-m-times.png img/dfa/repeat-between-n-and-m-times.png

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
