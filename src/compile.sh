#!/bin/bash
ghc -Wall -Wno-name-shadowing --make Main.hs -outputdir /tmp/ghc-build -o grimoire && \
rm -rf /tmp/ghc-build
