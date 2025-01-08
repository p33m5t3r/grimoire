#!/bin/bash
ghc -Wall -Wno-name-shadowing --make Tests.hs -outputdir /tmp/ghc-build -o runTests && \
rm -rf /tmp/ghc-build
./runTests
rm runTests

