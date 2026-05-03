# ASP.NET Core Trailing Block DSL (.rvnproj)

This sample builds a small routing DSL on top of ASP.NET Core Minimal APIs.

The DSL uses Raven trailing blocks in two places:

- `app.Route("/todos") { ... }` creates a route group, similar to Ktor's `route`; it is not tied to one HTTP method.
- `GET(...) { ... }` and `POST(...) { ... }` use trailing blocks as lightweight handler delegates.

The interop layer stays ordinary Raven code. `Route`, `MapDsl`, and `Named` are extension methods, and the mapper lowers descriptors to `WebApplication.MapGroup`, `MapGet`, `MapPost`, and `WithName`.

The intended typed-handler shape is:

```raven
app.Route("/todos") {
    GET<int>("/{id:int}", func (id: int) => store.Describe(id)).Named("todos-get")
}
```

That is not enabled in the runnable sample yet because the current compiler emitter fails when typed handler delegates are constructed inside this descriptor path. See "Proposed compiler changes" below.

## Build and run

From this folder:

```bash
dotnet run --project ../../../src/Raven.Compiler --framework net11.0 --property WarningLevel=0 -- AspNetTrailingBlockDsl.rvnproj --run
```

Then browse to `http://localhost:5000/`.

## Endpoints

- `GET /`
- `GET /health`
- `GET /todos`
- `GET /todos/first`
- `POST /todos/seed`

Quick smoke test:

```bash
curl http://localhost:5000/
curl http://localhost:5000/health
curl http://localhost:5000/todos
curl http://localhost:5000/todos/first
curl -X POST http://localhost:5000/todos/seed
```

## Proposed compiler changes

- Allow source-defined verb factory functions such as `GET<T>(pattern, handler)` to participate in nested trailing-block overload resolution inside builder blocks.
- Investigate trailing blocks on identifiers or route-scoped callable members so a route group can eventually support `GET { ... }` for the current route prefix.
- Fix emission for function expressions and trailing blocks passed through generic descriptor constructors; current failures report a missing parameter builder for the handler parameter.
- Keep Raven function type signatures as the public DSL surface, while reliably lowering them to the underlying `Func<>` or `Action<>` delegate type when passed to ASP.NET Core APIs.
- Improve overload resolution for ASP.NET Core metadata extensions such as `WithTags`, where Raven currently reports the generic and non-generic overloads as ambiguous.
