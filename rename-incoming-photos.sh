#!/bin/bash

set -e

cd $HOME/incoming

find . -iname '*HEIC' -exec /home/carlo/work/github/camera-scripts/HEIC_to_jpg.sh {} \;

find . -iname '*jpg' -exec exifautotran {} \;

find . -type f -exec chmod 644 {} \;

prename 's/JPG/jpg/g' *JPG

rename-photos

cd
