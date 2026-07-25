# Custom analyzer project

This sample attaches a project-specific analyzer to a normal Raven project:

```xml
<Analyzer
  Include="extension/bin/$(Configuration)/$(TargetFramework)/CustomAnalyzer.dll" />
```

The analyzer itself is written in Raven under `extension/`. Its syntax callback
uses a typed `let ... else` guard to recognize `ClassDeclarationSyntax`.

The companion `ProjectReference` builds the extension before Raven compilation.
`ReferenceOutputAssembly="false"` keeps the analyzer assembly out of the
application's normal metadata references.

Run:

```bash
dotnet build samples/projects/custom-analyzer/CustomAnalyzerSample.rvnproj \
  --property WarningLevel=0
```

The build succeeds with the custom diagnostic:

```text
warning SAMPLE001: Type name 'customer_record' should start with an uppercase letter.
```

Rename the class in `src/main.rvn` to `CustomerRecord` and build again to remove
the warning.
