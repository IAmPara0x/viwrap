#!/usr/bin/bash

set -xe

BASE_DIR=$(pwd)

DIRS=("src" "viwrap-capture" "test")

for DIR in ${DIRS[@]}; do

  find "${BASE_DIR}/${DIR}" -name "*.hs" | xargs -L1 brittany --write-mode=inplace -v
  cd "${BASE_DIR}/${DIR}" && stylish-haskell -i -r -v .

done


