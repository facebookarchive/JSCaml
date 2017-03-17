#!/bin/bash

js2z="1"
../bin/flow check --all

cd .out
for f in *.js.ml
do
  ocamlopt -o ${f%%.*} -I ../../runtime/_build -I ../../runtime/regexp/_build ../../runtime/_build/js_runtime.cmxa ../../runtime/_cobj/*.o ../../runtime/regexp/_build/*.o -cc g++ ${f%%.*}.js.ml
done
