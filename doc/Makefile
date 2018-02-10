DRAFT = report
FINAL = final

BIB_SOURCES = report.bib
PAPER_SOURCES := $(wildcard *.tex)
DRAFT_SOURCES = report.tex $(PAPER_SOURCES)
FINAL_SOURCES = final.tex $(PAPER_SOURCES)

SVG_FIGS = $(wildcard figs/*.svg)
SVG_PDF = $(SVG_FIGS:.svg=.pdf)

ALL_FIGS = $(SVG_PDF)

all: draft # final
draft: $(DRAFT).pdf
final: $(FINAL).pdf

%.pdf : %.svg
	inkscape -D -z --file="$(CURDIR)/$<" --export-pdf="$(CURDIR)/$@" --export-latex

$(DRAFT).pdf : $(DRAFT_SOURCES) $(ALL_FIGS) $(DRAFT).bbl
	pdflatex $(DRAFT)

$(DRAFT).aux : $(DRAFT_SOURCES) $(ALL_FIGS)
	pdflatex $(DRAFT)

$(DRAFT).bbl : $(DRAFT_SOURCES) $(BIB_SOURCES) $(DRAFT).aux
	bibtex $(DRAFT)

$(FINAL).pdf : $(FINAL_SOURCES) $(ALL_FIGS) $(FINAL).bbl
	pdflatex $(FINAL)

$(FINAL).aux : $(FINAL_SOURCES) $(ALL_FIGS)
	pdflatex $(FINAL)

$(FINAL).bbl : $(FINAL_SOURCES) $(BIB_SOURCES) $(FINAL).aux
	bibtex $(FINAL)

check:
	./check.sh

clean:
	rm -f $(DRAFT).pdf $(DRAFT).out $(DRAFT).dvi $(DRAFT).bbl $(DRAFT).aux $(DRAFT).blg $(DRAFT).log $(FINAL).pdf $(FINAL).out $(FINAL).dvi $(FINAL).bbl $(FINAL).aux $(FINAL).blg $(FINAL).log $(SVG_PDF)

.PHONY: clean check
