if [ -x "$(command -v code)" ]; then
    code --install-extension hackwaly.ocaml
fi

opam init
opam install depext
opam depext conf-m4.1
opam install merlin
opam install ocp-indent