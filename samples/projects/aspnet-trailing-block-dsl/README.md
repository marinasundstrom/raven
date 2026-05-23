# ASP.NET Core Receiver DSL (.rvnproj)

This sample builds a small Ktor-shaped routing DSL as a thin wrapper over ASP.NET Core Minimal APIs.

The sample keeps the interop layer ordinary Raven code:

- `app.Routing { ... }` opens a receiver block whose members map directly to ASP.NET Core route registration.
- `Route("/todos") { ... }` creates a `MapGroup` route group and opens another receiver block for nested endpoints.
- `Get("/{id:int}") { id => ... }` uses a parameterized trailing block when the route handler needs typed inputs.
- `Get(...) { ... }` and `Post(...) { ... }` use trailing blocks as lightweight zero-argument handler delegates.
- `.Configure { Name = "..." }` uses a `[Receiver]` trailing block so endpoint members are in scope while configuring metadata.

The interop layer stays ordinary Raven code. `Routing` and `Configure` are extension methods, while `Route`, `Get`, and `Post` are receiver members on small wrapper objects. Those wrappers immediately delegate to `WebApplication.MapGroup`, `MapGet`, `MapPost`, and `WithName`.

The typed-handler shape omits the generic type argument at the call site; Raven infers `GET<int>` from the trailing-block parameter:

```raven
app.Routing {
    Route("/todos") {
        Get("/{id:int}") { id =>
            val todo = store.Find(id)

            if todo is null {
                return "Not found"
            } else {
                return todo.ToLine()
            }
        }.Configure {
            Name = "todos-get"
        }
    }
}
```

## Build and run

From this folder:

```bash
dotnet run --project ../../../src/Raven.Compiler --framework net10.0 --property WarningLevel=0 -- AspNetTrailingBlockDsl.rvnproj --run
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

## Related sample

For builder-backed UI-style composition, see `samples/projects/mock-ui-builder-dsl`.
