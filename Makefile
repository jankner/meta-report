report.pdf: report.tex
	pdflatex report.tex
	bibtex report.aux
	pdflatex report.tex
	pdflatex report.tex

report.tex: report.md Makefile bib.bib paper.tex template.tex
	pandoc report.md -N --toc -o report.tex --tab-stop=2 --template=template.tex --bibliography=bib.bib --biblatex --chapters

paper.tex: paper.md Makefile bib.bib
	pandoc paper.md -o paper.tex --bibliography=bib.bib --biblatex --chapters
