# ASP.NET Core Minimal API (.rvnproj)

This sample shows a Raven project targeting an ASP.NET Core Minimal API endpoint.

Project file:

- `AspNetMinimalApi.rvnproj`
- Framework reference: `Microsoft.AspNetCore.App`

Source file:

- `src/main.rvn`

## Build

From this folder:

```bash
dotnet run --project ../../../src/Raven.Compiler --property WarningLevel=0 -- AspNetMinimalApi.rvnproj --run
```

This emits output to `bin/` by default for project builds.

## Run

```bash
dotnet bin/AspNetMinimalApi.dll
```

Then browse to `http://localhost:5000/`.

Available endpoints:

- `GET /` -> `Hello from Raven Minimal API`
- `GET /ping` -> `pong`
- `GET /async` -> `Hello from async MapGet`
- `POST /submit` -> `submitted`
- `POST /submit-async` -> `submitted async`

Quick smoke test:

```bash
curl http://localhost:5000/
curl http://localhost:5000/ping
curl http://localhost:5000/async
curl -X POST http://localhost:5000/submit
curl -X POST http://localhost:5000/submit-async
```
