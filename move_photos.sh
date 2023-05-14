#!/bin/bash

set -e

for f in `ls 20*jpg 20*JPG 20*jpeg 20*JPEG 20*png 20*PNG 20*heic 20*HEIC`
do
    yyyy=`echo ${f} | cut -f 1 -d '-'`
    mm=`echo ${f} | cut -f 2 -d '-'`

    yyyymm="${yyyy}-${mm}"

    dest=$HOME/annex-camera/photos/${yyyymm}/

    (mkdir ${dest} &> /dev/null) || true

    mv -vb $f $dest
    # echo $f $dest
done
