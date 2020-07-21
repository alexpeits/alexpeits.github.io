#!/usr/bin/env bash

cabal new-build
cabal new-exec peits -- "$@"

while inotifywait -e close_write config.yml posts/*.md pages/*.md templates/* static/**/*; do
    cabal new-exec peits -- "$@"
done

