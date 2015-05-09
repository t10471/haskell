#!/bin/bash

is_valid_args() {
    if [ "${#2}" -eq 0 ] ;then
        echo "$1"
        kill -14 "$$"
    fi
    return 0
}

run() {
    cp -r $B $P
    cd $P
    local hs=${P}1.hs
    local cabal=${P}.cabal
    mv $B_HS $hs
    mv $B_C  $cabal
    rm -rf tags
    sed -i -e "s/${B}/${P}/"     $cabal
    sed -i -e "s/\[\[text\]\]/${D}/" $cabal
    sed -i -e "s/${B}/${P}/"     memo.txt
    cabal sandbox init
    cabal install --only-dependencies
}

B="00base"
B_HS="${B}1.hs"
B_C="${B}.cabal"
P=$1
D=$2
is_valid_args "need projct name" $P
is_valid_args "need description" $D
run

