.PHONY: ghcid

watch:
	cabal new-exec site watch

build:
	cabal new-exec site build

clean:
	cabal new-exec site clean

cabal-configure:
	nix-shell -A shell --command 'cabal new-configure -w $$(which ghc)'

ghcid:
	ghcid -a --command="cabal new-repl"

push-cache:
	nix-store -qR --include-outputs $$(nix-instantiate default.nix -A site) | cachix push alexpeits-travis
