import { dotnet } from './_framework/dotnet.js';

globalThis.ravenCallback = document.querySelector('#callback');
globalThis.ravenLocation = globalThis.location.href;

try {
    const { setModuleImports, runMain } = await dotnet
        .withDiagnosticTracing(false)
        .withApplicationArgumentsFromQuery()
        .create();

    setModuleImports('raven', {
        setGreeting(message, onRendered) {
            document.querySelector('#app').textContent = message;
            onRendered('JavaScript called back into Raven.');
        }
    });

    await runMain();
} catch (error) {
    document.querySelector('#app').textContent = `Raven failed to start: ${error}`;
    throw error;
}
