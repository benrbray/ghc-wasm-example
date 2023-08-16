// based on fourmolu-wasm by Brandon Chinn
// https://github.com/fourmolu/fourmolu/blob/8aa2200fb38345d624d9682a051682094017bf8e/web/worker/index.js

import { Fd, WASI } from '@bjorn3/browser_wasi_shim'
import "./workerApi";

type WasmApi = {
	fibonacci_hs(n: number): number;
}

function debug(...args: unknown[]) {
	console.log(`%c[worker]`, "color: green", ...args);
}

////////////////////////////////////////////////////////////////////////////////

async function main() {
	const wasmPath = "/public/main.wasm";
	const wasm = await initWebAssembly(fetch(wasmPath));
	const hs = wasm.instance.exports as WasmApi;

	console.log(hs.fibonacci_hs(500));

	onmessage = (evt) => {
		const request = evt.data as WorkerRequest;

		debug("received request from main", request);

		if(request.tag === "addOne") {
			const result = hs.fibonacci_hs(request.value);
			debug(`result = ${result}`);

			respondSuccess();
			return;
		} else {
			respondUnknown();
		}
	};

	// send initial message indicating worker is ready
	respondReady();
}

////////////////////////////////////////////////////////////////////////////////

function respond(response: WorkerResponse): void {
	debug("posting response", response);
	postMessage(response);
}

function respondSuccess(): void {
	respond({ tag: "workerSuccess" });
}

function respondReady(): void {
	respond({ tag: "workerReady" });
}

function respondUnknown(): void {
	respond({ tag: "workerUnknownRequest" });
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