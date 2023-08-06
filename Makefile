all: main.native main.byte
.PHONY: all

main.native: parser.ml main.ml
	ocamlopt -O3 $^ -o $@

main.byte: parser.ml main.ml
	ocamlc $^ -o $@

clean:
	rm -Rf {main,parser}.{o,cmi,cmo,cmx} main.{native,byte}
.PHONY: clean
