#!/usr/bin/env bash

usage() {
    echo "Build the blog & other helpers

Usage:
    -b, --build    Build
    -w, --watch    Automatically watch for changes and serve on port 9999
    -h, --help     This help text
"
}

copy-files() {
    cp -R result/* site/
    chmod -R +w site/
}

run-nix() {
    nix-build
}

cleanup() {
    mkdir -p site
    rm -rf site/*
}

build() {
    cleanup
    run-nix
    copy-files
}

develop() {
    find . \( -name '*.md' -or -name '*.css' -or -name '*.nix' \) -and ! -wholename '*/site/*' -and ! -wholename '*/result/*' | entr -c -d ./run.sh --develop-cmd
}

develop-cmd() {
    run-nix
    tree result
}

serve() {
    (cd site && python3 -m http.server 9999)
}

watch() {
    find . \( -name '*.md' -or -name '*.css' -or -name '*.nix' \) -and ! -wholename '*/site/*' -and ! -wholename '*/result/*' | entr -c -d -r ./run.sh --watch-cmd
}

watch-cmd() {
    build
    serve
}

case "$1" in
    -b | --build)
        build
        ;;
    -d | --develop)
        develop
        ;;
    --develop-cmd)
        develop-cmd
        ;;
    -s | --serve)
        serve
        ;;
    -w | --watch)
        watch
        ;;
    --watch-cmd)
        watch-cmd
        ;;
    -c | --cleanup)
        cleanup
        ;;
    -h | --help)
        usage
        ;;
    *)
        usage
        exit 1
        ;;
esac
