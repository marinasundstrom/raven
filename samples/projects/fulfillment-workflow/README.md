# Fulfillment Workflow (.rvnproj)

This sample showcases a realistic Raven domain workflow around warehouse fulfillment.
It uses:

- domain records and discriminated unions
- `Option<T>` and `Result<T, E>` instead of `null` / thrown domain errors
- result propagation in async flows
- pattern matching over domain decisions
- list and dictionary comprehensions, including pattern-targeted comprehensions
- .NET interop through `System.Text.Json`, `Task`, `Guid`, and `DateTimeOffset`

Project file:

- `FulfillmentWorkflow.rvnproj`

Source file:

- `src/main.rvn`

## Build

From this folder:

```bash
dotnet run -f net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- FulfillmentWorkflow.rvnproj --no-emit
```

## Run

```bash
dotnet run -f net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- FulfillmentWorkflow.rvnproj --run
```
