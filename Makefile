TEST=test

all: $(TEST)

test: *.ml
	ocamlc -o test cnf.ml dpll.ml generator.ml test.ml

clean:
	@echo "[CLEAN]"
	-rm -f $(TEST)
	-find . -name "*.cm[oix]" -exec rm {} \;
	-find . -name "*.cm[t]" -exec rm {} \;
	-find . -name "*.cmt[i]" -exec rm {} \;
