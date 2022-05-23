#! /usr/bin/env bash

# Issue some variables as usual!
HERE=`dirname "$(readlink -f "$BASH_SOURCE")"`
APP_PATH=$(readlink -f $HERE/..)
MDFILE="${HERE}/md2man.md"
LBL="${HERE}/../deb/lbl"


# Grab the application name and version from ../package.yaml
APP_VERSION=$(cat $APP_PATH/package.yaml | $LBL grep version :: split :: :1)
APP_NAME=$(cat $APP_PATH/package.yaml | $LBL grep name :: split :: :1)
APP_AUTHOR=$(cat $APP_PATH/package.yaml | $LBL \
  grep author :: split :: :1~ :: split '"')
APP_MAINTAINER=$(cat $APP_PATH/package.yaml | $LBL \
  grep maintainer :: split :: :1~ :: split '"')
APP_DESCRIPTION=$(cat $APP_PATH/package.yaml | $LBL \
  grep '^description' :: split :: :1~ :: split '"')
DEB_NAME="${APP_NAME}_${APP_VERSION}-${REVISION}-${ARCH}"


# Build a very own md2man
stack install --local-bin-path $HERE $APP_PATH


SYN=$(./md2man --help | head -n3 | sed 's/^Usage: //g' | sed 's/^       //g')
DESC=$(./md2man --help | tail -n10)

echo "# md2man

## Name

**md2man** - ${APP_DESCRIPTION} 

## Synopsis

${SYN}

## Description

${DESC}

" > $MDFILE


cat README.md >> $MDFILE

./md2man $MDFILE \
  -o md2man.1 \
  --author "$APP_AUTHOR" \
  --email "$APP_MAINTAINER" \
  --revision "$APP_VERSION" \
  --section 1 \
  --book 'md2man manual page'
