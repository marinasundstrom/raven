# Vehicle Costs API (.rvnproj)

This sample is a Raven ASP.NET Core Web API on .NET 11 Preview 6. It tracks
vehicles, stores a Raven `union VehicleStatus`, and predicts monthly fuel costs
from recent fuel-consumption entries.

## Project file

- `VehicleCostsApi.rvnproj`
- Framework reference: `Microsoft.AspNetCore.App`
- Package references:
  - `Microsoft.AspNetCore.OpenApi`
  - `Microsoft.EntityFrameworkCore`
  - `Npgsql.EntityFrameworkCore.PostgreSQL`

## Domain

- `VehicleEntity`
- `FuelConsumptionRecord`
- `union VehicleStatus`
  - `Operational`
  - `Maintenance`
  - `Decommissioned`

The important part is that the union crosses the domain, persistence, and
response layers directly:

- response contracts expose `VehicleStatus`, without a DTO mirror
- EF Core persists `VehicleEntity.Status` directly through the PostgreSQL `jsonb` `Status` column
- ASP.NET Core describes the response union with an OpenAPI `anyOf` schema

EF's `HasConversion` uses a tagged converter privately when reading and writing
the `jsonb` column because all three cases are JSON objects. HTTP responses use
.NET 11's native union serialization, so clients receive the active case value
without that persistence discriminator. The Minimal API sample also shows a
union used directly as a request body.

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
  "typicalMonthlyDistanceKm": 1800
}
```

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
