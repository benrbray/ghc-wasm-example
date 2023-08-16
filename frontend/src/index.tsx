/* @refresh reload */
import { render } from 'solid-js/web'

// import { WASI, File, OpenFile, PreopenDirectory } from "@bjorn3/browser_wasi_shim";

import WasmWorker from "./worker/worker?url"

import './index.css'

const root = document.getElementById('root')

const App = function () {
  return <div>
    <button onClick={() => postMessage("BUTTON!")}>Post Message</button>
  </div>
}

window.onload = function() {
  render(() => <App />, root!)
}

// let wasmPath = "/public/haskell.wasm"

////////////////////////////////////////////////////////////////////////////////

// async function initWebAssembly(source: Promise<Response>) {
//   const wasi = new WASI([], [], [])
//   const wasm = await WebAssembly.instantiateStreaming(source, {
//     wasi_snapshot_preview1: wasi.wasiImport,
//   })
//   wasi.inst = wasm.instance as any
//   return wasm
// }

// const wasm = await initWebAssembly(fetch(wasmPath))
// const hs = wasm.instance.exports

// console.log(wasm);
// console.log(hs);
// console.log((hs as any).fibonacci_hs(500));

////////////////////////////////////////////////////////////////////////////////

const myWorker = new Worker(WasmWorker, { type: "module" });

function postMessage(message: string) {
  myWorker.postMessage(message);
  console.log("Message posted to worker");
}

myWorker.onmessage = (ev) => {
  console.log("message received from worker", ev);
}