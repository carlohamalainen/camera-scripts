#!/bin/bash

set -e
set -x

f=$1
g=$1"_rot90"

rm medium/$f
convert -rotate 90 "img/"$f "img/"$g
mv "img/"$g "img/"$f

