#!/usr/bin/env bash

cabal new-build

while inotifywait -e close_write posts/*.md pages/*.md; do
    cabal new-exec alexpeits-exe -- "$@"
done

