OPTS = '-pdf'

all:
	latexmk -lualatex main.tex $(OPTS)

continue:
	make all OPTS=' -pvc'

clean:
	latexmk -C main.tex
