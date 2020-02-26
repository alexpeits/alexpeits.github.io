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
    eval "nix-build $NIX_ARGS"
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

update-statue() {
    if [ -z "$1" ]; then
        rev="$(curl -s https://api.github.com/repos/alexpeits/statue/commits/master | jq -r .sha)"
    else
        rev="$1"
    fi
    nix-prefetch-git --url https://github.com/alexpeits/statue.git --rev "$rev" --quiet | tee statue.json
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
    -u | --update)
        update-statue "$2"
        ;;
    -h | --help)
        usage
        ;;
    *)
        usage
        exit 1
        ;;
esac
