/* @refresh reload */
import { render } from 'solid-js/web'
import WasmWorker from "./worker/worker?url"

import './index.css'

////////////////////////////////////////////////////////////////////////////////

const root = document.getElementById('root')

const App = function () {
  return <div>
    <button onClick={() => callWorker({ tag: "addOne", value: 5 })}>Post Message</button>
  </div>
}

window.onload = function() {
  initWorker();

  render(() => <App />, root!);
}

////////////////////////////////////////////////////////////////////////////////

let _worker: Worker;
let _currentRequest: Promise<WorkerResponse>;

/**
 * Communication with the web worker is based on `fourmolu-wasm` by Brandon Chinn.
 * https://github.com/fourmolu/fourmolu/blob/8aa2200fb38345d624d9682a051682094017bf8e/web/site/static/demo.js
 */
function initWorker() {
  _worker = new Worker(WasmWorker, { type: "module" });

  _currentRequest = new Promise((resolve) => {
    // wait for initial message indicating worker is ready
    _worker.onmessage = (evt) => {
      const response = evt.data as WorkerResponse;
      if(response.tag !== "workerReady") { return; }

      console.log("worker is ready");

      // start allowing requests
      resolve(response);
    }
  });

  callWorker({ tag: "addOne", value: 5 });
}

/** 
  * All calls to the web worker should be made through `callWorker`,
  * which chains requests as promises to ensure synchronous access.
  */
function callWorker(message: WorkerRequest): Promise<WorkerResponse> {
  const promise: Promise<WorkerResponse> =
    _currentRequest.then(() =>
      new Promise((resolve) => {
        console.log("[main] sending request to worker", message);
        _worker.postMessage(message);
        _worker.onmessage = (event) => resolve(event.data);
      })
    );

  _currentRequest = promise;
  return promise;
}