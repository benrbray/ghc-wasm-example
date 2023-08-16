/* @refresh reload */
import { render } from 'solid-js/web'

import { WASI, File, OpenFile, PreopenDirectory } from "@bjorn3/browser_wasi_shim";

// wasi-js doesn't seem to work
// import WASI from "wasi-js";
// import wasiBrowserBindings from "wasi-js/dist/bindings/browser"

import './index.css'
import App from './App'

const root = document.getElementById('root')

render(() => <App />, root!)

// async function readFile(url: string): Promise<ArrayBuffer> {
// 	let resp = await fetch(url);
// 	return await resp.arrayBuffer();
// 	// return await resp.arrayBuffer();
// }

let wasmPath = "/public/haskell.wasm"

////////////////////////////////////////////////////////////////////////////////

async function initWebAssembly(source: Promise<Response>) {
  const wasi = new WASI([], [], [])
  const wasm = await WebAssembly.instantiateStreaming(source, {
    wasi_snapshot_preview1: wasi.wasiImport,
  })
  wasi.inst = wasm.instance as any
  return wasm
}

const wasm = await initWebAssembly(fetch(wasmPath))
const hs = wasm.instance.exports

console.log(wasm);
console.log(hs);
console.log((hs as any).fibonacci_hs(500));


// // let args: string[] = []; // ["bin", "arg1", "arg2"];
// // let env = ["FOO=bar"];
// // let fds = [
// // 	new OpenFile(new File([])), // stdin
// // 	new OpenFile(new File([])), // stdout
// // 	new OpenFile(new File([])), // stderr
// // 	new PreopenDirectory(".", {
// // 		"example.c": new File(new TextEncoder().encode(`#include "a"`)),
// // 		"hello.rs": new File(new TextEncoder().encode(`fn main() { println!("Hello World!"); }`)),
// // 	}),
// // ];

// let wasi = new WASI(args, env, fds);

// let wasm = await WebAssembly.compileStreaming(fetch(wasmPath));

// let inst = await WebAssembly.instantiate(wasm, {
//     "wasi_snapshot_preview1": wasi.wasiImport,
// });

// console.log(inst)

// wasi.start(inst as any);