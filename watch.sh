#!/usr/bin/env bash

cabal new-build

while inotifywait -e close_write posts/*.md pages/*.md static/**/*; do
    cabal new-exec peits -- "$@"
done

