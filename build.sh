#!/usr/bin/env bash

if grep -q "//setup();" "./csrc/hfmu.c";
then sed -i "" 's/\/\/setup();/setup();/g' "./csrc/hfmu.c"
fi

cabal new-build
