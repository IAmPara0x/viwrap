#!/usr/bin/bash 

set -xe

find ./src -name "*.hs" | xargs -L1 brittany --write-mode=inplace -v
cd ./src && stylish-haskell -i -r -v .
