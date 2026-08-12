function withTimeout(promise, timeoutMs, timeoutMessage) {
    const timeout = new Promise((_, reject) =>
        setTimeout(() => reject(new Error(timeoutMessage)), timeoutMs));
    return Promise.race([promise, timeout]);
}

class DotnetWebWorkerClient {
    #worker;
    #pendingRequests = {};
    #requestId = 0;

    constructor(worker) {
        this.#worker = worker;
    }

    static create(initTimeoutMs, options = {}) {
        const worker = new Worker('_content/Raven.Playground.Worker/dotnet-web-worker.js', { type: 'module' });
        const initWorker = new Promise((resolve, reject) => {
            worker.addEventListener('error', event =>
                reject(new Error(event.message || 'Worker encountered an error')));
            worker.addEventListener('message', function onMessage(event) {
                if (event.data.type !== 'ready') return;
                worker.removeEventListener('message', onMessage);
                event.data.error ? reject(new Error(event.data.error)) : resolve();
            });
        });

        const dotnetJsUrl = DotnetWebWorkerClient.#resolveDotnetJsUrl();
        worker.postMessage({ type: 'init', dotnetJsUrl, assemblyName: options?.assemblyName ?? null });

        return withTimeout(initWorker, initTimeoutMs, 'Worker initialization timed out').then(() => {
            const client = new DotnetWebWorkerClient(worker);
            client.#setupMessageHandler();
            return client;
        }, error => {
            worker.terminate();
            throw error;
        });
    }

    static #resolveDotnetJsUrl() {
        const dotnetJsUrl = new URL('_framework/dotnet.js', document.baseURI).href;
        return import.meta.resolve?.(dotnetJsUrl) ?? dotnetJsUrl;
    }

    invoke(method, args, timeoutMs) {
        const id = ++this.#requestId;
        const invoke = new Promise((resolve, reject) => {
            this.#pendingRequests[id] = { resolve, reject };
            this.#worker.postMessage({ method, args, requestId: id });
        });

        return withTimeout(invoke, timeoutMs, `Worker method '${method}' timed out`).catch(error => {
            delete this.#pendingRequests[id];
            throw error;
        });
    }

    terminate() {
        this.#rejectAllPending('Worker terminated');
        this.#worker?.terminate();
        this.#worker = null;
    }

    #setupMessageHandler() {
        this.#worker.addEventListener('message', event => {
            if (event.data.type !== 'result') return;
            const request = this.#pendingRequests[event.data.requestId];
            if (!request) return;

            delete this.#pendingRequests[event.data.requestId];
            event.data.error
                ? request.reject(new Error(event.data.error))
                : request.resolve(event.data.result);
        });
        this.#worker.addEventListener('error', event =>
            this.#rejectAllPending(event.message || 'Worker error'));
    }

    #rejectAllPending(errorMessage) {
        for (const id in this.#pendingRequests) {
            this.#pendingRequests[id].reject(new Error(errorMessage));
            delete this.#pendingRequests[id];
        }
    }
}

export function create(initTimeoutMs, options) {
    return DotnetWebWorkerClient.create(initTimeoutMs, options);
}
