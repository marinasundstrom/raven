let workerExports = {};
let startupError = null;

async function initialize(dotnetJsUrl, assemblyName) {
    try {
        const { dotnet } = await import(dotnetJsUrl);
        const { getAssemblyExports, getConfig } = await dotnet.create();
        const mainAssemblyName = getConfig().mainAssemblyName;

        workerExports = { ...await getAssemblyExports(mainAssemblyName) };
        if (assemblyName && assemblyName !== mainAssemblyName) {
            workerExports = { ...workerExports, ...await getAssemblyExports(assemblyName) };
        }

        self.postMessage({ type: 'ready' });
    } catch (error) {
        startupError = error?.message ?? String(error);
        console.error('[Worker] Failed to initialize .NET:', error);
        self.postMessage({ type: 'ready', error: startupError });
    }
}

self.addEventListener('message', async event => {
    if (event.data.type === 'init') {
        await initialize(event.data.dotnetJsUrl, event.data.assemblyName);
        return;
    }

    const { method, args, requestId } = event.data;
    try {
        if (Object.keys(workerExports).length === 0) {
            throw new Error(startupError || 'Worker .NET runtime not loaded');
        }

        const fn = method.split('.').reduce((value, part) => value?.[part], workerExports);
        if (typeof fn !== 'function') throw new Error(`Method not found: ${method}`);

        const result = await fn(...args);
        self.postMessage({ type: 'result', requestId, result });
    } catch (error) {
        self.postMessage({
            type: 'result',
            requestId,
            error: error?.message ?? String(error),
        });
    }
});
