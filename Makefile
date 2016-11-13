TARGET = src/maze
LIBS = -I, /usr/lib/ocaml/
FLAGS = -j 0 -r -use-ocamlfind -pkgs llvm, llvm, llvm.analysis, llvm.bitwriter, llvm.linker, llvm.target
OCAMLBUILD = ocamlbuild
OPAM = opam config env

clean: 
	ocamlbuild -clean
	rm -rf testall.log *.diff maze scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.o a.out

	
