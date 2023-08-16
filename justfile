set shell := ["bash", "-uc"]

deps-wasm:
	source /home/benjamin/.ghc-wasm/env

build-wasm: deps-wasm
	cd haskell && ./build.sh
	cp ./haskell/build/haskell-wasm.wasm ./frontend/public/main.wasm

dev: build-wasm
	npm run dev