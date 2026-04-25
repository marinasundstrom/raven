# Vehicle Costs API (.rvnproj)

This sample is a Raven ASP.NET Core Web API that tracks vehicles, stores a Raven `union VehicleStatus`, and predicts monthly fuel costs from recent fuel-consumption entries.

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

The important bit is the mapping approach:

- the domain uses a real Raven `union`
- EF Core persists `VehicleEntity.StatusJson` as PostgreSQL `jsonb`
- `EncodeAsJson` and `DecodeFromJson` translate between the Raven union and a JSON DTO shape

That keeps the public model union-based while still using JSON storage in the database.

## Run PostgreSQL

From this folder:

```bash
docker compose up -d
```

## Build and run

From this folder:

```bash
dotnet run --project ../../../src/Raven.Compiler --property WarningLevel=0 -- VehicleCostsApi.rvnproj --run
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
  "status": {
    "kind": "operational",
    "driverName": "Robin",
    "sinceUtc": "2026-04-01T08:00:00Z",
    "currentOdometerKm": 88410
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
