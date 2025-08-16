# Generate DocFX


## Prerequisite

### Build Generator

If DocFX cannot resolve `Generator`, build it manually:

Run this from directory `tools/Generator`:

```
dotnet build -c Release
```

### Collect metadata

Run this to gather metadata from projects:

```
docfx metadata
```

## Build and serve site

Run this in the `docs` directory:

```
docfx docfx.json --serve
```

Then launch [http://localhost:8080](http://localhost:8080)