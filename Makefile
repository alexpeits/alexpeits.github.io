NIXPKGS?=nixpkgs-unstable
HOOGLE_PORT?=8888

NIX_SHELL=nix-shell --argstr pkgs ${NIXPKGS}

ghcid:
	${NIX_SHELL} --run 'ghcid -a'

build:
	${NIX_SHELL} --run 'cabal new-build'

exec:
	${NIX_SHELL} --run 'cabal new-build && cabal new-exec alexpeits-exe -- ${ARGS}'

hoogle:
	${NIX_SHELL} --run 'hoogle server --port ${HOOGLE_PORT} --local'

serve:
	cd _build && python -m http.server 8080

list-nixpkgs:
	jq -n '[inputs | keys[]]' nix/sources.json

build-info:
	git log -1 --format=%H%n%cd > _build/build-info.txt

copy-nix-files:
	mkdir -p site
	rm -rf site/*
	cp -R result/_build/* site/
	chmod -R +w site/
