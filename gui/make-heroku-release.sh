#!/bin/bash

# Fail fast and fail hard.
set -eo pipefail

HEROKU_APP=deeplearning-breakthrough

TMP=$(mktemp -d heroku.XXXX)
trap "rm -rf $TMP" EXIT INT TERM

BASE=$(pwd)
echo TMP=$TMP
echo BASE=$BASE

# prepare sdist for deeplearning package
cd ..
cabal-dev sdist 
cd $BASE
cp ../dist/deeplearning-*.tar.gz $BASE/prepared-sdists

# prepare sdist for this package
rm -f dist/*tar.gz
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
git push -f heroku master # forced push

# clean up: go back to BASE, TMP removal handled by trap.
cd $BASE
