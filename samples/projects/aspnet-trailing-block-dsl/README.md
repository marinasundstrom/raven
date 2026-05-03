# ASP.NET Core Trailing Block DSL (.rvnproj)

This sample builds a small routing DSL on top of ASP.NET Core Minimal APIs.

The sample keeps the runtime path intentionally small:

- `app.Route("/todos") { ... }` creates a route group, similar to Ktor's `route`; it is not tied to one HTTP method.
- `GET("/{id:int}", func (id: int) => ...)` uses a normal function-expression argument when the route handler needs typed inputs.
- Root utility endpoints use plain ASP.NET Core Minimal API calls so the sample stays focused on the DSL mechanics.

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
- `GET /todos/{id:int}`

Quick smoke test:

```bash
curl http://localhost:5000/
curl http://localhost:5000/health
curl http://localhost:5000/todos/1
```

## Proposed compiler changes

- Fix repeated trailing-block emission in the same scope before expanding the sample back to multiple DSL endpoints per group.
- Investigate trailing blocks on identifiers or route-scoped callable members so a route group can eventually support `GET { ... }` for the current route prefix.
- Keep Raven function type signatures as the public DSL surface, while reliably lowering them to the underlying `Func<>` or `Action<>` delegate type when passed to ASP.NET Core APIs.
- Improve overload resolution for ASP.NET Core metadata extensions such as `WithTags`, where Raven currently reports the generic and non-generic overloads as ambiguous.
