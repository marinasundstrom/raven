import { dotnet } from './_framework/dotnet.js';

globalThis.ravenCallback = document.querySelector('#callback');
globalThis.ravenLocation = globalThis.location.href;

try {
    const { setModuleImports, getAssemblyExports, getConfig, runMain } = await dotnet
        .withDiagnosticTracing(false)
        .withApplicationArgumentsFromQuery()
        .create();

    setModuleImports('raven', {
        setGreeting(message, onRendered) {
            document.querySelector('#app').textContent = message;
            onRendered('JavaScript called back into Raven.');
        }
    });

    const config = getConfig();
    const exports = await getAssemblyExports(config.mainAssemblyName);
    document.querySelector('#export').textContent =
        exports.BrowserInterop.FormatGreeting('from the browser');

    await runMain();
} catch (error) {
    document.querySelector('#app').textContent = `Raven failed to start: ${error}`;
    throw error;
}
