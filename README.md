# `ghc-wasm-example`

A working example demonstrating the use of:

* [`ghc-wasm-meta`](https://gitlab.haskell.org/ghc/ghc-wasm-meta) for compiling Haskell code to a [WASI module](https://wasi.dev/)
* [`browser-wasi-shim`](https://github.com/bjorn3/browser_wasi_shim) for running the compiled WASI module in the browser

At the moment, the build is based mostly on Brandon Chinn's [`fourmolu-wasm`](https://github.com/fourmolu/fourmolu/tree/main/web/fourmolu-wasm) setup, in addition to the following resources:

* Haskell Discourse, ["Javascript & WebAssembly backend"](https://discourse.haskell.org/t/javascript-webassembly-backend/6787)
* `ghc-wasm-meta` @ GitLab, Issue #1, ["Allow disabling `head.hackage` in `setup.sh`"](https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/issues/1) (and [this commit](https://github.com/fourmolu/fourmolu/pull/334/commits/0a186b1c60ce6a279b62794886ab7aea11ed65ae), [this workflow](https://github.com/fourmolu/fourmolu/blob/0a186b1c60ce6a279b62794886ab7aea11ed65ae/.github/workflows/web.yml))

## Setup

Install `ghc-wasm-meta` according to the ["Getting Started without Nix"](https://gitlab.haskell.org/ghc/ghc-wasm-meta#getting-started-without-nix).  I used all of the default settings.  Then, populate your environment with `source ~/.ghc-wasm/env`.

```bash
$ wasm32-wasi-cabal --version
cabal-install version 3.10.1.0
compiled using version 3.10.1.0 of the Cabal library

$ wasm32-wasi-ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.9.20230810
```

The build scripts are organized as a [`justfile`](https://github.com/casey/just).

```
just build-wasm
```

Once the Haskell project has been built, you can run the frontend in development mode with:

```
cd frontend
npm run dev
```