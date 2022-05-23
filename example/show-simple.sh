#! /usr/bin/env bash


HERE=`dirname "$(readlink -f "$BASH_SOURCE")"`
SRC=$HERE/simple.md
DEST="${SRC%.*}.1.man"


stack install --local-bin-path $HERE $APP_PATH


$HERE/md2man \
  --author Alice\
  --email alice@example.com\
  --section 2\
  --revision 3.2\
  -o $DEST $SRC

man $DEST
