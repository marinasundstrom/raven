# ASP.NET Core Minimal API

This Raven project implements a small pet-shelter API with ASP.NET Core on
.NET 11 Preview 7. It focuses on application-shaped examples instead of
isolated endpoint-binding tests.

The `Pet`, `PetLookup`, and `VaccinationStatus` types are Raven unions.
ASP.NET Core accepts `PetLookup` directly as a request body, writes the other
unions directly as responses, and describes their cases with `anyOf` in the
generated OpenAPI document. JSON contains only the active case value; it does
not need a discriminator.

The sample also includes named async and streaming handlers:

- `GET /pets/{id}` returns one of the `Pet` cases.
- `POST /pets/find` accepts a `PetLookup` containing either an integer ID or a
  `PetName` object.
- `GET /pets/{id}/vaccinations` returns a `VaccinationStatus` asynchronously.
- `GET /pets` streams `Pet` values and observes request cancellation.
- `GET /openapi/v1.json` serves the generated OpenAPI document.

`src/Domain.rvn` declares the model and endpoint handlers directly in the
`AspNetMinimalApi.Domain` namespace. `src/Program.rvn` imports those namespace
members and focuses only on application composition and route mapping. Unlike
the equivalent C# organization, Raven does not require a static container
class merely to share named handlers across files.

## Run

From this folder:

```bash
dotnet run --project AspNetMinimalApi.rvnproj --property WarningLevel=0
```

Then use `sample.http`, or try:

```bash
curl http://localhost:5000/pets/1
curl http://localhost:5000/pets
curl -X POST http://localhost:5000/pets/find \
  -H 'Content-Type: application/json' \
  -d '{"name":"Luna"}'
curl http://localhost:5000/openapi/v1.json
```

The project references `Microsoft.AspNetCore.App` and the .NET 11 Preview 7
`Microsoft.AspNetCore.OpenApi` package.
