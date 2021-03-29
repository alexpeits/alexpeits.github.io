#!/usr/bin/env bash

cabal new-build
cabal new-exec peits -- "$@"

if command -v inotifywait; then
  while inotifywait -e close_write \
      app/*.hs \
      config.yml \
      posts/*.md \
      pages/*.md \
      lists/*.yml \
      drafts/*.md \
      templates/*.mustache \
      static/**/*; do
    cabal new-build
    cabal new-exec peits -- "$@"
  done
elif command -v entr; then
  find . -type f \
      -wholename './app/*.hs' \
      -or -wholename './config.yml' \
      -or -wholename './posts/*.md' \
      -or -wholename './pages/*.md' \
      -or -wholename './lists/*.yml' \
      -or -wholename './drafts/*.md' \
      -or -wholename './templates/*.mustache' \
      -or -wholename './static/*/**' | entr -d -c sh -c "cabal new-build; cabal new-exec peits -- $@"
else
  echo "Install inotify wait or entr"
  exit 1
fi

