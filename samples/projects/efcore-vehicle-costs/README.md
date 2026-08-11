# Vehicle Costs API (.rvnproj)

This sample is a Raven ASP.NET Core Web API on .NET 11 Preview 7. It tracks
vehicles, stores a Raven `union VehicleStatus`, records how much each vehicle
can carry, and predicts monthly fuel costs from recent consumption entries.

## Project file

- `VehicleCostsApi.rvnproj`
- Framework reference: `Microsoft.AspNetCore.App`
- Package references:
  - `Microsoft.AspNetCore.OpenApi`
  - `Microsoft.EntityFrameworkCore`
  - `Npgsql.EntityFrameworkCore.PostgreSQL`

The SDK, ASP.NET Core framework, and OpenAPI packages target Preview 7. The EF
Core and PostgreSQL provider pair remains on its latest compatible .NET 11
Preview 6 packages until Npgsql publishes a Preview 7 build.

## Domain

- `VehicleEntity`
- `FuelConsumptionRecord`
- `Money` and `Currency`
- `Weight` and `WeightUnit`
- `union VehicleStatus`
  - `Operational`
  - `Maintenance`
  - `Decommissioned`

The sample uses domain types instead of passing ambiguous decimal values
through the API. Fuel costs and predictions are returned as `Money`, while
vehicle payload capacity uses `Weight`. This keeps the sample focused on the
small fleet model without prematurely implementing dispatch or delivery
planning.

The important part is that Raven unions cross the domain, persistence, and
response layers directly:

- response contracts expose `VehicleStatus`, without a DTO mirror
- EF Core persists `VehicleEntity.Status` directly through the PostgreSQL `jsonb` `Status` column
- ASP.NET Core describes the response union with an OpenAPI `anyOf` schema

## Union persistence boundary

Raven unions emit the .NET union shape (`UnionAttribute`, `IUnion.Value`, and
typed creation/extraction members) proposed by the C# language team. That makes
the domain model interoperable without designing it around EF Core.

EF Core does not currently provide a union-specific relational mapping. This
sample therefore uses EF's documented model/provider conversion pattern:
`VehicleStatus` remains the model type, while `HasConversion<string>` calls
`VehicleStatusJson` to store and restore tagged JSON. The `$case` discriminator
is private persistence data and makes every case unambiguous.

This tradeoff is deliberate: EF treats a value-converted property as opaque, so
queries cannot navigate into individual status payload fields. If those fields
must be indexed or queried independently, map a separate persistence model or
adopt EF complex-type JSON mapping when it can represent the required union
shape.

HTTP responses use .NET 11's native union serialization, so clients receive the
active case value without the persistence discriminator. The relevant upstream
designs are the
[C# unions proposal](https://github.com/dotnet/csharplang/blob/main/proposals/unions.md)
and EF Core's
[value conversion guidance](https://learn.microsoft.com/ef/core/modeling/value-conversions).

## Run PostgreSQL

From this folder:

```bash
docker compose up -d
```

## Build and run

From this folder:

```bash
dotnet run --project VehicleCostsApi.rvnproj --property WarningLevel=0
```

## Endpoints

- `GET /vehicles`
- `GET /vehicles/{id}`
- `GET /vehicles/by-status/{kind}`
- `GET /vehicles/{id}/cost-prediction`
- `POST /vehicles`
- `POST /vehicles/{id}/fuel-consumptions`

## Example payload

```json
{
  "registrationNumber": "RAV-303",
  "model": "Skoda Octavia",
  "fuelType": "Diesel",
  "typicalMonthlyDistanceKm": 1800,
  "payloadCapacity": {
    "amount": 750,
    "unit": 0
  }
}
```

For the enum-backed JSON field, `WeightUnit` uses `0` for kilograms and `1`
for pounds.

The API initializes a new vehicle as operational. A response contains the
active union case directly:

```json
{
  "status": {
    "driverName": "Unassigned",
    "sinceUtc": "2026-07-26T00:00:00+00:00",
    "currentOdometerKm": 0
  }
}
```

## Containerize the API

```bash
docker build -f samples/projects/efcore-vehicle-costs/Dockerfile -t vehicle-costs-api .
docker run --rm -p 8080:8080 \
  -e ConnectionStrings__VehicleCosts="Host=host.docker.internal;Port=5432;Database=vehicle_costs;Username=postgres;Password=postgres" \
  vehicle-costs-api
```
