#!/bin/bash

export Z3_VERSION=4.5.0

wget https://github.com/Z3Prover/z3/releases/download/z3-$Z3_VERSION/z3-$Z3_VERSION-x64-ubuntu-14.04.zip
unzip z3-$Z3_VERSION-x64-ubuntu-14.04.zip
cp z3-$Z3_VERSION-x64-ubuntu-14.04/bin/z3 ~/.local/bin
cp z3-$Z3_VERSION-x64-ubuntu-14.04/bin/*.so ~/.local/lib
