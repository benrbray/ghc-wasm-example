set shell := ["bash", "-uc"]

build-wasm:
	cd haskell && ./build.sh
	cp ./haskell/build/ghc-wasm-example.wasm ./frontend/public/main.wasm

dev: build-wasm
	npm run dev