#!/bin/bash

cp *.ml ../flow/src/stubs
cd ../flow
make
mkdir -p ../bin
cp bin/flow ../bin
rm src/stubs/*
git reset --hard
