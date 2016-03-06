#!/bin/bash

PS4='($LINENO)+ '
set -x
set -e

rm -fr .cabal-sandbox cabal.sandbox.config dist

stack build --haddock
