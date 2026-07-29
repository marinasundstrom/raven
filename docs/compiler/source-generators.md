# Source generators

Source generators produce additional Raven source files from a project
compilation. They are separate from macros: generators run as a workspace
compilation step, while invocable compile-time macros may be added as a
language feature later.

For the user-facing overview of analyzers and source generators, see
[Extend a Raven project](extending-projects.md).

Implement `ISourceGenerator` and register the instance, type, or containing
assembly through a `GeneratorReference`. Generator references are project-level
compilation inputs, separate from diagnostic analyzer references:

```raven
import Raven.CodeAnalysis.*

class ModelGenerator : ISourceGenerator {
    func Initialize(context: GeneratorInitializationContext) {}

    func Execute(context: GeneratorExecutionContext) {
        context.AddSource("GeneratedModel", "class GeneratedModel {}")
    }
}

let updatedProject = project.AddGeneratorReference(
    GeneratorReference(ModelGenerator()))
```

Raven projects can load a compiled generator assembly declaratively:

```xml
<ItemGroup>
  <SourceGenerator Include="extensions/MyGenerators.dll" />
</ItemGroup>
```

The workspace runs generators before returning the project compilation.
Generated syntax trees therefore participate in binding, diagnostics, emit,
project references, and subsequent analyzer execution.

`GeneratorDriver` can also be used directly by compiler hosts. Its run result
contains each generated source, generator diagnostic, and generator exception.
An unhandled generator exception is converted to `RVNGEN001` and does not crash
the workspace.

Hint names are relative paths. The `.rvn` extension is supplied when omitted,
and duplicate hint names from the same generator are rejected.
