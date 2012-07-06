#!/bin/bash

echo "*** Compiling llvm-clojure"
ghc -fllvm main.hs

echo "*** Running llvm-clojure on sample input file"
./main

if [ $? -ne 0 ]
then
    exit 1
fi

echo "*** Compiling generated LLVM assembly with llc"
llc -O0 -o output.S output.ll && clang -fcatch-undefined-behavior -ftrapv -O0 -o output output.S

if [ $? -ne 0 ]
then
    exit 1
fi

echo "*** Running compiled sample program"
./output
