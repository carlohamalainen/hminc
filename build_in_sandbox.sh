#!/bin/bash

PS4='($LINENO)+ '
set -x
set -e

export sandbox=/scratch/sandboxes/hminc

rm -fr $sandbox cabal.sandbox.config dist

cabal sandbox init --sandbox=${sandbox}

cabal install c2hs
cabal install --haddock-hyperlink-source --dependencies-only --force-reinstalls
cabal install --haddock-hyperlink-source
