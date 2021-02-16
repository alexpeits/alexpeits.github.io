NIXPKGS?=nixpkgs
HOOGLE_PORT?=8888
PORT?=8080
EXE?=peits

NIX_SHELL=nix-shell --argstr pkgs ${NIXPKGS}

ghcid:
	${NIX_SHELL} --run "ghcid -a --command='cabal new-repl exe:${EXE}'"

build:
	${NIX_SHELL} --run 'cabal new-build'

hoogle:
	${NIX_SHELL} --run 'hoogle server --port ${HOOGLE_PORT} --local'

serve:
	cd _build && python -m http.server ${PORT}

watch:
	./watch.sh

copy-nix-files:
	mkdir -p site
	rm -rf site/*
	cp -R result/_build/* site/
	chmod -R +w site/

build-info:
	git log -1 --format=%H%n%cd > site/build-info.txt
