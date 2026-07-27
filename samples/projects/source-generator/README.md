# Source generator project

This sample attaches a source generator to a normal Raven project:

```xml
<SourceGenerator
  Include="extension/bin/$(Configuration)/$(TargetFramework)/RouteGenerator.dll" />
```

The generator itself is written in Raven under `extension/`. It walks Raven
syntax trees and uses typed `if let` matching to find class declarations.

The generator finds classes whose names end in `Controller` and contributes a
corresponding type in the `Generated` namespace. For `HomeController`, it adds:

```raven
namespace Generated

class HomeControllerRoute {}
```

The maintained `src/Controllers.rvn` file uses `Generated.HomeControllerRoute`, proving
that generated source participates in normal binding without being written
into the source directory.

Run:

```bash
dotnet build samples/projects/source-generator/SourceGeneratorSample.rvnproj \
  --property WarningLevel=0
```

Remove the `<SourceGenerator>` item and build again to see the missing generated
type diagnostic.
