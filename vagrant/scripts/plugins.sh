if [ -x "$(command -v code)" ]; then
    code --install-extension hackwaly.ocaml
fi

opam init
opam install depext
opam depext conf-m4.1
opam install merlin.2.5.0
opam install ocp-indent
opam install user-setup
opam user-setup install