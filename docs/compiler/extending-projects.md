# Extend a Raven project

Raven projects can be extended with custom diagnostics and generated source
code. These extensions let a project enforce its own rules and derive
repetitive code without changing the files that developers maintain.

There are two complementary extension types:

| Extension | Use it to | Effect on the project |
|---|---|---|
| Analyzer | Find mistakes, enforce conventions, or highlight project-specific concerns. | Reports diagnostics against existing code. |
| Source generator | Derive declarations, adapters, registries, or other repetitive code. | Adds generated Raven source to the compilation. |

An extension is ordinary .NET code built against `Raven.CodeAnalysis`. A Raven
project can load the compiled extension assembly from its `.rvnproj` file. A
compiler host can also attach extensions directly to a workspace project.

## Choose the right extension

Use an analyzer when the developer should make the decision or edit:

- flag an API that your application should not call;
- enforce naming or architectural rules;
- identify likely defects;
- recommend a safer or clearer construct.

Use a source generator when the result follows mechanically from project
code:

- create a registry from annotated types;
- generate serialization or mapping code;
- add strongly typed accessors;
- derive repetitive members from a model declaration.

Neither extension rewrites a developer's source file. An analyzer describes a
problem at a source location. A generator contributes separate syntax trees to
the current compilation snapshot.

## How extensions participate in a build

For each project snapshot, Raven:

1. parses the project's maintained source files;
2. runs registered source generators;
3. adds the generated source to the compilation;
4. binds and checks the complete compilation;
5. runs analyzers against the resulting compilation;
6. emits the program when no blocking diagnostics remain.

Generated code can therefore declare symbols used by maintained source.
Compilation-wide analyzer actions can inspect the complete result, including
generated syntax trees. If a source file or generator reference changes, the
workspace creates a new compilation snapshot and replaces obsolete generated
trees. The files in the source tree remain unchanged.

## Add a custom diagnostic

An analyzer derives from `DiagnosticAnalyzer`. It declares the diagnostics it
can report and registers focused callbacks for the syntax, symbols, or
operations it needs to inspect.

```csharp
using System.Collections.Immutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;

public sealed class AvoidLegacyApiAnalyzer : DiagnosticAnalyzer
{
    private static readonly DiagnosticDescriptor Rule = DiagnosticDescriptor.Create(
        id: "APP001",
        title: "Avoid the legacy API",
        description: null,
        helpLinkUri: "https://example.test/rules/APP001",
        messageFormat: "Use the current API instead of '{0}'.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => [Rule];

    public override void Initialize(AnalysisContext context)
    {
        context.EnableConcurrentExecution();
        context.RegisterSyntaxNodeAction(AnalyzeName, SyntaxKind.IdentifierName);
    }

    private static void AnalyzeName(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not IdentifierNameSyntax name ||
            name.Identifier.ValueText != "LegacyApi")
        {
            return;
        }

        context.ReportDiagnostic(Diagnostic.Create(
            Rule,
            name.GetLocation(),
            name.Identifier.ValueText));
    }
}
```

Attach the analyzer through an analyzer reference:

```csharp
project = project.AddAnalyzerReference(
    new AnalyzerReference(new AvoidLegacyApiAnalyzer()));
```

In an `.rvnproj`, reference the compiled analyzer assembly with an `Analyzer`
item:

```xml
<ItemGroup>
  <Analyzer Include="extensions/MyProjectRules.dll" />
</ItemGroup>
```

Analyzer diagnostics flow through Raven's normal diagnostic pipeline. Their
severity can be configured, and they can appear in command-line and editor
diagnostic output.

External analyzers should use a stable diagnostic prefix owned by the
extension. The `RAV` prefix is reserved for Raven's built-in diagnostics.

## Generate additional source

A source generator implements `ISourceGenerator`. Its execution context
provides the current compilation and accepts one or more generated Raven
sources.

```csharp
using Raven.CodeAnalysis;

public sealed class RouteTableGenerator : ISourceGenerator
{
    public void Initialize(GeneratorInitializationContext context)
    {
    }

    public void Execute(GeneratorExecutionContext context)
    {
        context.AddSource(
            "RouteTable",
            """
            namespace Generated

            class RouteTable {
                public static val Count: int = 3
            }
            """);
    }
}
```

Attach it through a generator reference:

```csharp
project = project.AddGeneratorReference(
    new GeneratorReference(new RouteTableGenerator()));
```

In an `.rvnproj`, use the separate `SourceGenerator` item:

```xml
<ItemGroup>
  <SourceGenerator Include="extensions/MyProjectGenerators.dll" />
</ItemGroup>
```

`RouteTable.rvn` becomes part of the project compilation, but Raven does not
write it into the source directory. The hint name identifies the generated
source; Raven adds the `.rvn` extension when it is omitted.

Generators may also report diagnostics when generation cannot continue. An
unhandled generator failure is isolated and reported as `RVNGEN001` instead of
crashing the workspace.

## Package extensions

`AnalyzerReference` and `GeneratorReference` can each be created from:

- an extension instance;
- an extension type with a parameterless constructor;
- an assembly containing discoverable extension types.

Using separate references is intentional. Generators change the inputs to
binding and emit, so changing them invalidates the project compilation.
Analyzers observe a compilation and contribute diagnostics without changing
its source.

An assembly may contain both extension types, but a host registers it
separately for each role:

```csharp
var extensionAssembly = typeof(RouteTableGenerator).Assembly;

project = project
    .AddGeneratorReference(new GeneratorReference(extensionAssembly))
    .AddAnalyzerReference(new AnalyzerReference(extensionAssembly));
```

## Keep extensions predictable

Extensions run repeatedly while a project is edited. Design them as
deterministic, cancellation-aware transformations:

- derive output only from the supplied compilation and explicit extension
  inputs;
- use stable diagnostic IDs and generated hint names;
- avoid writing into the user's source tree;
- do not depend on execution order between extensions;
- let cancellation stop long-running work promptly;
- report diagnostics for expected project problems rather than throwing.

This keeps command-line builds, editor diagnostics, and generated project
state consistent with one another.

## Runnable samples

The repository includes complete projects whose extensions are written in
Raven itself:

- [Custom analyzer sample](https://github.com/marinasundstrom/raven/tree/main/samples/projects/custom-analyzer)
  reports a project-specific naming diagnostic during a normal Raven build.
- [Source generator sample](https://github.com/marinasundstrom/raven/tree/main/samples/projects/source-generator)
  generates a route type that maintained Raven source consumes.
- [Syntax Tree API sample](https://github.com/marinasundstrom/raven/tree/main/samples/projects/syntax-tree-api)
  parses Raven source and walks its syntax nodes from a Raven application.

Each sample builds its extension assembly through a `ProjectReference`, then
loads that output through the corresponding Raven project item.

## Analyzers, generators, and macros

Analyzers observe code. Source generators add compilation-wide derived code.
Macros are a separate language mechanism for explicit compile-time behavior at
an invocation site.

Choose an analyzer for feedback, a generator for project-wide derived
declarations, and a macro when the source itself should explicitly invoke a
compile-time operation.
