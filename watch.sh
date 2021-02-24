#!/usr/bin/env bash

cabal new-build
cabal new-exec peits -- "$@"

if command -v inotifywait; then
  while inotifywait -e close_write config.yml posts/*.md pages/*.md templates/* static/**/*; do
    cabal new-exec peits -- "$@"
  done
elif command -v entr; then
  find . -name '*.md' -or -name '*.css' -or -name '*.js' -or -name '*.mustache' -type f | entr -c cabal new-exec peits
else
  echo "Install inotify wait or entr"
  exit 1
fi

