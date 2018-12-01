shell:
	nix-shell
run:
	# cabal configure
	cabal run
	# cabal v1-run
watch:
	ls src/*.hs | entr cabal build
build:
	# cabal v1-build
	cabal build
