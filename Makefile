.PHONY: ghcid

watch:
	cabal new-exec site watch

build:
	cabal new-exec site build

cabal-configure:
	nix-shell --command 'cabal new-configure -w $$(which ghc)'

ghcid:
	ghcid -a --command="cabal new-repl"
