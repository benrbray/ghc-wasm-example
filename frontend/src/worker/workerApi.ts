type Tag<T extends string> = { tag: T };

/* ---- request ----------------------------------------- */

type WorkerRequest = AddOne | ToUpper

type AddOne = Tag<"addOne">  & { value: number };
type ToUpper= Tag<"toUpper"> & { value: string };

/* ---- response ---------------------------------------- */

type WorkerResponse = WorkerReady | WorkerSuccess | WorkerUnknownRequest;

type WorkerReady = Tag<"workerReady">;
type WorkerSuccess = Tag<"workerSuccess">;
type WorkerUnknownRequest = Tag<"workerUnknownRequest">;
