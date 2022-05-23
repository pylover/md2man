#! /usr/bin/env bash

# Issue some variables as usual!
HERE=`dirname "$(readlink -f "$BASH_SOURCE")"`
APP_PATH=$(readlink -f $HERE/..)
REVISION=1
LBL="${HERE}/lbl"
EXECFILE="${HERE}/md2man"
ARCH=$(dpkg-architecture -qDEB_BUILD_ARCH)


# Build a very own md2man
stack install --local-bin-path $HERE $APP_PATH


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


# Create target directories
WORKING_DIR=$HERE/$DEB_NAME
if [ -d $WORKING_DIR ]; then
  echo "Removing previous build working directory: ${WORKING_DIR}"
  rm -r $WORKING_DIR
fi

mkdir -p $WORKING_DIR/usr/local/bin
mkdir -p $WORKING_DIR/usr/share/man/man1
mkdir -p $WORKING_DIR/DEBIAN

# Copy the binary
cp $EXECFILE $WORKING_DIR/usr/local/bin

# Control file
echo "\
Package: ${APP_NAME}
Version: ${APP_VERSION}
Architecture: ${ARCH}
Maintainer: ${APP_AUTHOR} ${APP_MAINTAINER}
Description: ${APP_DESCRIPTION}" \
  > $WORKING_DIR/DEBIAN/control


# Dependencies
cd $WORKING_DIR
cp -r DEBIAN debian
echo "Source:" >> debian/control
dpkg-shlibdeps -O usr/local/bin/md2man >> DEBIAN/control
rm -rf debian
cd ..

# Manual page
cd ../man
make clean md2man.1
cd ../deb
cp ../man/md2man.1 $WORKING_DIR/usr/share/man/man1


# Build
dpkg-deb --build --root-owner-group $WORKING_DIR
