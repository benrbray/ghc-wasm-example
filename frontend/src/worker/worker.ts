// based on fourmolu-wasm by Brandon Chinn
// https://github.com/fourmolu/fourmolu/blob/8aa2200fb38345d624d9682a051682094017bf8e/web/worker/index.js

import { Fd, WASI } from '@bjorn3/browser_wasi_shim';
import "./workerApi";

type Ptr = number;

type WasmApi = {
	malloc(n: number): Ptr; // ???
	free(ptr: Ptr): void; // ???
	getString(ptr: Ptr): Ptr;
	getStringLen(ptr: Ptr): number;
	freeStringWithLen(ptr: Ptr): void; //
	runFibonacci(n: number): number;
	runToUpper(ptr: Ptr, len: number): Ptr;
}

////////////////////////////////////////////////////////////////////////////////

const defaultConsoleLog = console.log;
console.log = function(...args: unknown[]) {
	defaultConsoleLog(`%c[worker]`, "color: green", ...args);
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();

////////////////////////////////////////////////////////////////////////////////

type Haskell = WasmApi & WebAssembly.Exports & { memory: WebAssembly.Memory };

const withBytesPtr = (
	haskell: Haskell,
	bytes: Uint8Array,
	callback: (inputPtr: number, inputLen: number) => void
) => {
	const len = bytes.byteLength
	const ptr = haskell.malloc(len)
	try {
		const memory = haskell.memory as WebAssembly.Memory
		new Uint8Array(memory.buffer, ptr, len).set(bytes)
		callback(ptr, len)
	} finally {
		haskell.free(ptr)
	}
}


async function main() {
	const wasmPath = "/public/main.wasm";
	const wasm = await initWebAssembly(fetch(wasmPath));
	const haskell = wasm.instance.exports as Haskell;

	console.log(haskell);

	onmessage = (evt) => {
		const request = evt.data as WorkerRequest;

		console.log("received request from main", request);

		if(request.tag === "addOne") {
			const result = haskell.runFibonacci(request.value);
			console.log(`result = ${result}`);

			respondSuccess();
			return;
		} else if(request.tag === "toUpper") {
			runToUpper(haskell, request);
		} else {
			respondUnknown();
		}
	};

	// send initial message indicating worker is ready
	respondReady();
}

function decodeStringWithLen(haskell: Haskell, ptr: Ptr): string {
	try {
		const outputPtr = haskell.getString(ptr);
		const outputLen = haskell.getStringLen(ptr);
		const outputBytes = new Uint8Array(haskell.memory.buffer, outputPtr, outputLen);
		const output = decoder.decode(outputBytes);

		return output;
	} finally {
		haskell.freeStringWithLen(ptr);
	}
}

function runToUpper(
	haskell: Haskell,
	request: ToUpper
) {
	const inputBytes = encoder.encode(request.value);

	withBytesPtr(haskell, inputBytes, (inputPtr, inputLen) => {
		const resultPtr = haskell.runToUpper(inputPtr, inputLen);
		const resultStr = decodeStringWithLen(haskell, resultPtr);
		console.log(`result: ${resultStr}`);
	});

	respondSuccess();
	return;
}

////////////////////////////////////////////////////////////////////////////////

function respond(response: WorkerResponse): void {
	console.log("posting response", response);
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