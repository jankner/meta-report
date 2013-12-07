report.pdf: report.tex
	pdflatex report.tex

report.tex: report.md Makefile bib.bib
	pandoc report.md -N --toc -o report.tex --tab-stop=2 -s --bibliography=bib.bib
