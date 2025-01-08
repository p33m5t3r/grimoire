#!/bin/bash
ghc --make Main.hs -outputdir /tmp/ghc-build -o grimoire && \
rm -rf /tmp/ghc-build
