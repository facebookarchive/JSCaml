#!/bin/bash

ocamlbuild js_runtime.cmxa
mkdir -p _cobj
cd _cobj
ocamlopt -c -I ../regexp/_build ../js_time.c ../js_regexp.c
