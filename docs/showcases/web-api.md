# Compose an ASP.NET Core API

This sample shows Raven acting as an ordinary ASP.NET Core application
language. `Program.rvn` remains a small composition root while named handler
functions live in another source file.

```raven
import AspNetMinimalApi.Domain.*
import Microsoft.AspNetCore.Builder.*

let builder = WebApplication.CreateBuilder(args)
builder.Services.AddOpenApi()

use app = builder.Build()
app.MapGet("/pets/{id}", FindPet)
app.MapGet("/pets", StreamPets)
app.MapPost("/pets/find", LookupPet)

app.Run()
```

## What the sample shows

- Raven calls the normal ASP.NET Core builder, services, routing, and lifetime
  APIs directly.
- `use` makes disposal of the built application explicit.
- Namespace-level functions such as `FindPet` and `StreamPets` can be imported
  from `Domain.rvn` and passed directly as route handlers.
- Application composition does not require a static container class for those
  handlers.

The complete pet-shelter sample adds records and unions for request and
response models, asynchronous handlers, streaming results, cancellation, JSON,
and generated OpenAPI.

Continue with [building web applications](../workloads/web-api.md) for project
layout, complete handlers, run commands, and the checked-in sample.
