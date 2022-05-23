#! /usr/bin/env bash


HERE=`dirname "$(readlink -f "$BASH_SOURCE")"`
SRC=$HERE/strdup.md
DEST="${SRC%.*}.3.man"


stack install --local-bin-path $HERE $APP_PATH


$HERE/md2man \
  --section 3\
  --revision "GNU"\
  --book "Linux Programmer's Manual"\
  -o $DEST $SRC

man $DEST
