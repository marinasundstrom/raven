# Build a Web API with ASP.NET Core

Raven can use ASP.NET Core directly. This guide builds a small pet-shelter
Minimal API with ordinary route mapping and dependency injection, while using
Raven records, unions, patterns, async functions, and top-level functions for
the domain layer.

The complete project lives in
[`samples/projects/aspnet-minimal-api`](https://github.com/marinasundstrom/raven/tree/main/samples/projects/aspnet-minimal-api).
It currently targets the .NET 11 preview SDK selected by the repository.

## What you will build

The API provides:

- `GET /pets/{id}` to return a pet by ID
- `POST /pets/find` to find a pet by ID or name
- `GET /pets/{id}/vaccinations` for an asynchronous status lookup
- `GET /pets` to stream pets with request cancellation
- `GET /openapi/v1.json` for the generated OpenAPI document

## Project layout

```text
aspnet-minimal-api/
├── AspNetMinimalApi.rvnproj
├── sample.http
└── src/
    ├── Program.rvn
    └── Domain.rvn
```

`Program.rvn` is the composition root. It creates the web application and maps
routes. `Domain.rvn` contains the API models and named handler functions.

Raven does not require a class merely to share functions across files. A
top-level function can live in a namespace, be imported into `Program.rvn`, and
be passed directly to ASP.NET Core as a route handler. This keeps the entry
point short as the application grows.

## Compose the application in `Program.rvn`

```raven
import AspNetMinimalApi.Domain.*
import Microsoft.AspNetCore.Builder.*
import Microsoft.Extensions.DependencyInjection.*

let builder = WebApplication.CreateBuilder(args)
builder.Services.AddOpenApi()

use app = builder.Build()
app.MapOpenApi()

app.MapGet("/", GetApiInfo)
app.MapGet("/pets/{id}", FindPet)
app.MapPost("/pets/find", LookupPet)
app.MapGet("/pets/{id}/vaccinations", CheckVaccinations)
app.MapGet("/pets", StreamPets)

app.Run()
```

The builder, dependency-injection container, route mapping, and application
lifetime are the familiar ASP.NET Core APIs. Raven's `use` binding makes
disposal of the built application explicit.

## Put handlers and models in `Domain.rvn`

Start the file with a namespace and the imports required by its handlers:

```raven
namespace AspNetMinimalApi.Domain

import System.Collections.Generic.*
import System.Runtime.CompilerServices.*
import System.Threading.*
import System.Threading.Tasks.*
```

Handlers are ordinary named functions. They do not need a static controller or
utility class:

```raven
func FindPet(id: int) -> Pet {
    match id {
        1 => Dog("Rex", "Labrador")
        2 => Cat("Luna", 9)
        _ => Bird("Pip", true)
    }
}

func LookupPet(lookup: PetLookup) -> Pet {
    match lookup {
        int id => FindPet(id)
        PetName(let name) => match name {
            "Rex" | "rex" => FindPet(1)
            "Luna" | "luna" => FindPet(2)
            _ => FindPet(3)
        }
    }
}
```

Asynchronous and streaming handlers use the corresponding .NET task and async
enumerable types:

```raven
async func CheckVaccinations(id: int) -> Task<VaccinationStatus> {
    await Task.Delay(25)

    if id == 1 {
        return VaccinationStatus.Current("2027-06-01")
    }

    return VaccinationStatus.Due("Rabies")
}

async func StreamPets(
    [EnumeratorCancellation] cancellationToken: CancellationToken
) -> IAsyncEnumerable<Pet> {
    yield FindPet(1)
    await Task.Delay(100, cancellationToken)
    yield FindPet(2)
    await Task.Delay(100, cancellationToken)
    yield FindPet(3)
}
```

The domain types use records for product-shaped data and unions for values that
can take one of several closed shapes:

```raven
record class Dog(val Name: string, val Breed: string)
record class Cat(val Name: string, val LivesRemaining: int)
record class Bird(val Name: string, val CanTalk: bool)

union Pet(Dog | Cat | Bird)

record class PetName(val Name: string)
union PetLookup(int | PetName)

union VaccinationStatus {
    case Current(nextDueDate: string)
    case Due(vaccine: string)
}
```

ASP.NET Core accepts `PetLookup` as a request body and serializes the other
unions as responses. The generated OpenAPI document describes the alternatives
with `anyOf`.

## Build and run

From the repository root:

```bash
dotnet run \
  --project samples/projects/aspnet-minimal-api/AspNetMinimalApi.rvnproj \
  --property WarningLevel=0
```

Then use the checked-in `sample.http` file or call the endpoints directly:

```bash
curl http://localhost:5000/pets/1
curl http://localhost:5000/pets
curl -X POST http://localhost:5000/pets/find \
  -H 'Content-Type: application/json' \
  -d '{"name":"Luna"}'
curl http://localhost:5000/openapi/v1.json
```

## What is Raven-specific?

The web host, routing, dependency injection, tasks, async streams, JSON, and
OpenAPI support come from .NET and ASP.NET Core. Raven contributes the source
model around them:

- plain namespace-level handler functions
- expression-oriented `match` forms
- records for concise data models
- unions for closed alternatives in requests and responses
- explicit resource lifetime through `use`

Continue with [domain modeling](../lang/domain-modeling.md) for guidance on
splitting functions and types as an application grows. Consult the
[language specification](../lang/spec/language-specification.md) only when you
need exact syntax or semantic rules.
