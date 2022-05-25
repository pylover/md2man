#! /usr/bin/env bash

# Issue some variables as usual!
HERE=`dirname "$(readlink -f "$BASH_SOURCE")"`
APP_PATH=$(readlink -f $HERE)
LBL="${HERE}/deb/lbl"
REVISION=1
ARCH=$(dpkg-architecture -qDEB_BUILD_ARCH)

# # Grab the application name and version from ../package.yaml
APP_VERSION=$(cat $APP_PATH/package.yaml | $LBL grep version :: split :: :1)
APP_NAME=$(cat $APP_PATH/package.yaml | $LBL grep name :: split :: :1)


cd deb
make clean build
DEB="${HERE}/deb/${APP_NAME}_${APP_VERSION}-${REVISION}-${ARCH}.deb"


# Create a github release
gh release create \
  --generate-notes \
  "v${APP_VERSION}" \
  $DEB  

