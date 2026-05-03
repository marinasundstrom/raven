# ASP.NET Core Trailing Block DSL (.rvnproj)

This sample builds a small routing DSL on top of ASP.NET Core Minimal APIs.

The sample keeps the interop layer ordinary Raven code:

- `app.Route("/todos") { ... }` creates a route group, similar to Ktor's `route`; it is not tied to one HTTP method.
- `GET("/{id:int}", func (id: int) => ...)` uses a normal function-expression argument when the route handler needs typed inputs.
- `GET(...) { ... }` and `POST(...) { ... }` use trailing blocks as lightweight zero-argument handler delegates.

The interop layer stays ordinary Raven code. `Route`, `MapDsl`, and `Named` are extension methods, and the mapper lowers descriptors to `WebApplication.MapGroup`, `MapGet`, `MapPost`, and `WithName`.

The typed-handler shape omits the generic type argument at the call site; Raven infers `GET<int>` from the function parameter:

```raven
app.Route("/todos") {
    GET("/{id:int}", func (id: int) => store.Describe(id)).Named("todos-get")
}
```

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
- `GET /todos/{id:int}`
- `POST /todos/seed`

Quick smoke test:

```bash
curl http://localhost:5000/
curl http://localhost:5000/health
curl http://localhost:5000/todos
curl http://localhost:5000/todos/1
curl -X POST http://localhost:5000/todos/seed
```

## Proposed compiler changes

- Investigate trailing blocks on identifiers or route-scoped callable members so a route group can eventually support `GET { ... }` for the current route prefix.
- Keep Raven function type signatures as the public DSL surface, while reliably lowering them to the underlying `Func<>` or `Action<>` delegate type when passed to ASP.NET Core APIs.
- Improve overload resolution for ASP.NET Core metadata extensions such as `WithTags`, where Raven currently reports the generic and non-generic overloads as ambiguous.
