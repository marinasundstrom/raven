import { dotnet } from './_framework/dotnet.js';

globalThis.ravenApp = document.querySelector('#app');
globalThis.ravenLocation = globalThis.location.href;

await dotnet
    .withDiagnosticTracing(false)
    .withApplicationArgumentsFromQuery()
    .run();
