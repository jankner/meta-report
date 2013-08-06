report.pdf: report.tex
	pdflatex report.tex

report.tex: report.md Makefile bib.bib
	pandoc report.md -o report.tex --tab-stop=2 -s --bibliography=bib.bib
