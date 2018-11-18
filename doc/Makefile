IMAGES = img/nfa/concat.png img/nfa/option.png img/nfa/kleene-star.png img/nfa/repeat-at-least-once.png img/nfa/repeat-n-times.png img/nfa/repeat-at-least-n-times.png img/nfa/repeat-at-most-m-times.png img/nfa/repeat-between-n-and-m-times.png img/nfa/alt.png img/nfa/neg.png

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
	rm img/nfa/*.png

conq.pdf: conq.tex dependencies
	$(PDFLATEX) conq.tex > /dev/null

dependencies: $(IMAGES)

img/nfa/%.png: img/nfa/%.dot
	$(DOT) $< -o $@
