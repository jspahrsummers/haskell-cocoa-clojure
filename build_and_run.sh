#!/bin/bash

ghc -fllvm main.hs

if [ $? -ne 0 ]
then
    exit 1
fi

./main "$@"

if [ $? -ne 0 ]
then
    exit 1
fi

if [ -z "$1" ]
then
    # no command-line arguments
    exit 0
fi

echo "*** Compiling generated LLVM assembly with llc"
llc -O0 -o output.S *.ll && clang -fcatch-undefined-behavior -ftrapv -O0 -o output output.S

if [ $? -ne 0 ]
then
    exit 1
fi

echo "*** Running compiled sample program"
./output
