#!/bin/bash

set -e

git checkout master
git add .
git commit -am "Pre-publish"
git push origin master

tmpdir=/tmp/blog

mv _site/* $tmpdir
git checkout -B gh-pages
rm -rf *
mv $tmpdir/* .
rm -rf $tmpdir
git add .
git commit -am "Publish"
git push origin gh-pages --force
git checkout master
