NIXPKGS?=nixpkgs
HOOGLE_PORT?=8888
PORT?=8080
EXE?=peits

ROOT_DIR=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

NIX_SHELL=nix-shell --argstr pkgs ${NIXPKGS}

clean:
	rm -rf _build/* .shake/

ghcid:
	${NIX_SHELL} --run "ghcid -a --command='cabal new-repl exe:${EXE}'"

build:
	${NIX_SHELL} --run 'cabal new-build'

hoogle:
	${NIX_SHELL} --run 'hoogle server --port ${HOOGLE_PORT} --local'

serve:
	cd _build && python -m http.server ${PORT}

watch: clean
	./watch.sh --highlight=prismjs

copy-nix-files:
	mkdir -p site
	rm -rf site/*
	cp -R result/_build/* site/
	chmod -R +w site/

build-info:
	git log -1 --format=%H%n%cd > site/build-info.txt

build-info-gh-actions:
	scripts/build-info-gh-actions.sh ${ROOT_DIR} site/build-info.json

serve-watch:
	yarn 'serve:watch'

check-links:
	yarn 'check-links:all'
