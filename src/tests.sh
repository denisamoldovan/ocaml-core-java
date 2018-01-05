eval $(opam config env)
ocamlfind  ocamlc -o test.o -package oUnit -linkpkg -g main.ml tests.ml
./test.o