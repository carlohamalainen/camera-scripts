#!/bin/bash

set -e

#for f in `ls *MOV *AAE *GIF *MP4 *mp4`
#do
#    dest=$HOME/annex-camera/movies/`date -r ${f} "+%Y-%m"`/
#
#    (mkdir ${dest} &> /dev/null) || true
#
#    mv -vb $f $dest
#done

for f in `ls 20*mov 20*MOV 20*mp4 *20*MP4`
do
    yyyy=`echo ${f} | cut -f 1 -d '-'`
    mm=`echo ${f} | cut -f 2 -d '-'`

    yyyymm="${yyyy}-${mm}"

    dest=$HOME/annex-camera/movies/${yyyymm}/

    (mkdir ${dest} &> /dev/null) || true

    mv -vb $f $dest
    # echo $f $dest
done

