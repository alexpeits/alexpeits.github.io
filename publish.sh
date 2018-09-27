#!/bin/bash

set -e

git checkout develop
git add .
git commit -am "Pre-publish"
git push origin develop

tmpdir=/tmp/blog
mkdir -p $tmpdir

mv _site/* $tmpdir
git checkout -B master
rm -rf *
mv $tmpdir/* .
rm -rf $tmpdir
git add .
git commit -am "Publish"
git push origin master --force
git checkout develop
