#!/bin/bash

set -e

cd $HOME/incoming

find . -iname '*jpg' -exec exifautotran {} \;

find . -type f -exec chmod 644 {} \;

prename 's/JPG/jpg/g' *JPG

rename-photos

cd
