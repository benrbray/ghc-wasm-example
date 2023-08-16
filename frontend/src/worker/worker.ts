// based on fourmolu-wasm by Brandon Chinn
// https://github.com/fourmolu/fourmolu/blob/8aa2200fb38345d624d9682a051682094017bf8e/web/worker/index.js

import { Fd, WASI } from '@bjorn3/browser_wasi_shim'

type WasmApi = {
	fibonacci_hs(n: number): number;
}

////////////////////////////////////////////////////////////////////////////////

onmessage = (e) => {
	console.log("Message received from main script", e.data);

	const workerResult = `Result: ${e.data[0] * e.data[1]}`;

	console.log("Posting message back to main script");
	postMessage(workerResult);
};

async function main() {
	const wasmPath = "/public/main.wasm";
	const wasm = await initWebAssembly(fetch(wasmPath));
	const hs = wasm.instance.exports as WasmApi;

	console.log(hs.fibonacci_hs(500));
}

////////////////////////////////////////////////////////////////////////////////

async function initWebAssembly(source: Promise<Response>) {
	const args: string[] = [];
	const env: string[]  = [];
	const fds: Fd[]      = [];
	const wasi = new WASI(args, env, fds);

	const wasm = await WebAssembly.instantiateStreaming(source, {
		wasi_snapshot_preview1: wasi.wasiImport,
	});

	wasi.inst = wasm.instance as any;
	return wasm;
}

main();