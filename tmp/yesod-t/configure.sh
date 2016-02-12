#!/bin/bash

ghc_version="7.8.4"

[ -z "$1" ] || ghc_version="$1"; shift

cabal configure --package-db=clear \
                --package-db=global \
                --package-db="$(stack path --snapshot-pkg-db)" \
                --package-db="$(stack path --local-pkg-db)" \
                --with-compiler="/usr/local/bin/ghc" \
                $@
