#!/bin/bash

is_valid_args() {
    if [ "${#2}" -eq 0 ] ;then
        echo "$1"
        kill -14 "$$"
    fi
    return 0
}

run() {
    grep -Ir --include="*.hs" $K .
}

cabal_run() {
    grep -Ir --include="*.cabal" $K .
}

K=$1
is_valid_args "need keyword" $K

if [[ "$2" == "cabal" ]] ;then
    cabal_run
else
    run
fi
