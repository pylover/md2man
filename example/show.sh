#! /usr/bin/env bash


if [ -z $1 ]; then
  >&2 echo "Enter file name."
  exit 1
else
  SRC=$1
fi

HERE=`dirname "$(readlink -f "$BASH_SOURCE")"`
SRC=`readlink -f $SRC`
DEST="${SRC%.*}.1"


stack install --local-bin-path $HERE $APP_PATH


$HERE/md2man \
  --author Alice\
  --email alice@example.com\
  --section 2\
  --app-version 3.2\
  -o $DEST $SRC

man $DEST
