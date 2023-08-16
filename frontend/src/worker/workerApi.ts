type Tag<T extends string> = { tag: T };

/* ---- request ----------------------------------------- */

type WorkerRequest = AddOne | Plus

type AddOne = Tag<"addOne"> & { value: number };
type Plus   = Tag<"plus">   & { a: number, b: number };

/* ---- response ---------------------------------------- */

type WorkerResponse = WorkerReady | WorkerSuccess | WorkerUnknownRequest;

type WorkerReady = Tag<"workerReady">;
type WorkerSuccess = Tag<"workerSuccess">;
type WorkerUnknownRequest = Tag<"workerUnknownRequest">;
