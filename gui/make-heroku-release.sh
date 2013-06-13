#!/bin/bash

# Fail fast and fail hard.
trap "rm -rfv $TMP" EXIT INT TERM

set -eo pipefail

HEROKU_APP=deeplearning-breakthrough

TMP=$(mktemp --tmpdir=/tmp -d heroku.XXXX)

BASE=$(pwd)
echo TMP=$TMP
echo BASE=$BASE

# prepare sdist for deeplearning package
mkdir -p $BASE/prepared-sdists
cd ..
cabal-dev sdist 
cd $BASE
cp ../dist/deeplearning-*.tar.gz $BASE/prepared-sdists

# prepare sdist for this package
rm -fv dist/*tar.gz
cabal-dev sdist

# unpack to TMP
tar xvaf dist/*tar.gz -C$TMP
mv $TMP/breakthrough-gui-*/* $TMP/
rm -rf $TMP/breakthrough-gui-*

# initialize git, add heroku remote
cd $TMP
git init
git add -A
git commit -m "automatic initial commit"
heroku git:remote --app $HEROKU_APP
time git push -f heroku master # forced push

# clean up: go back to BASE, TMP removal handled by trap.
cd $BASE
