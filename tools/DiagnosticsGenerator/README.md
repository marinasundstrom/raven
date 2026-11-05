# Diagnostic source generation

Run the generator from `src/Raven.CodeAnalysis`, where it reads
`DiagnosticDescriptors.xml`.

```sh
dotnet run --project ../../tools/DiagnosticsGenerator -- -f
dotnet run --project ../../tools/DiagnosticsGenerator -- --raven-catalog /path/to/CompilerDiagnostics.g.rvn
dotnet run --project ../../tools/DiagnosticsGenerator -- --raven-reporters /path/to/DiagnosticBagExtensions.g.rvn
```

The default mode writes the C# catalog, reporting helpers, and incremental stamp.
Each Raven mode writes only its requested output; use one output mode per command.
All modes share descriptor loading and placeholder ordering. Raven parameter names
are escaped so descriptor arguments can use language keywords without changing
metadata names.

The reporting output targets the .NET bootstrap port. It uses a partial static
`DiagnosticBagExtensions` class and explicit `ExtensionAttribute` methods (.NET
only) to preserve the existing C# container, receiver name, and ability to combine
handwritten overloads with generated members. It does not replace the handwritten
reporting overloads or establish native neoCLR hosting support.
