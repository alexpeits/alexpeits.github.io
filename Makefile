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
