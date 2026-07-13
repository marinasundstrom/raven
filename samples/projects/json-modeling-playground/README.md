# JSON Modeling Playground (.rvnproj)

This is a playground for modeling the structure of JSON with Raven records and
unions.

It uses a tagged `JsonValue` union to model scalar, object, array, and explicit
JSON null values, and a `JsonObject` record to hold object properties.
Serialization uses custom converters:

- `JsonValueConverter` serializes `JsonValue` cases as plain JSON values,
  including the `.Null` case as a JSON null token.
- `JsonObjectConverter` serializes the `JsonObject.Properties` dictionary as
  the JSON object body, similar to extension-data behavior but with a strongly
  typed `IDictionary<string, JsonValue>`.

Project file:

- `JsonModelingPlayground.rvnproj`

Source file:

- `src/main.rvn`

## Build

From this folder:

```bash
dotnet build JsonModelingPlayground.rvnproj --property WarningLevel=0
```

## Run

The compiler copies `Raven.Core.dll` beside the sample executable when invoked
with `--run`:

```bash
dotnet run --project JsonModelingPlayground.rvnproj --property WarningLevel=0
```

After that, the emitted app can also be run directly:

```bash
dotnet bin/Debug/net10.0/JsonModelingPlayground.dll
```
