# Source generators

Source generators produce additional Raven source files from a project
compilation. They are separate from macros: generators run as a workspace
or build-host compilation step, while macros expand an explicit invocation or
attached declaration inside the compiler.

For the user-facing overview of analyzers and source generators, see
[Extend a Raven project](extending-projects.md).

## When to use a generator instead of a macro

Choose a source generator when the output is derived from project-wide inputs
and naturally belongs in one or more generated documents. A registry assembled
from every annotated type is a typical generator task. Another is implementing
an authored partial declaration: the source declaration establishes the shape
or contract first, and a generated partial declaration supplies repetitive
members or implementation in a separate generated file. The generator receives
a compilation snapshot, contributes named source files, and is rerun by the
workspace or build host when its inputs change.

Choose a macro when the programmer explicitly asks for a local transformation
with `Name!(...)`, `Name! { ... }`, or an attached macro. Macro output occupies
the invocation or declaration's grammar position, and diagnostics, source
mapping, hover, navigation, and debugging can relate the expansion to that
authored site and its token body.

A generator should not search for invocations and emulate macro replacement;
a macro should not silently scan the whole compilation and emulate a generator.
See [Authoring Raven macros](../macro-authoring.md) for the corresponding macro
model and API.

The partial-declaration pattern is especially useful when consumers and editor
features must see the authored shape independently of generation. The generated
part augments that identity through Raven's normal partial-type merge; it does
not rewrite the authored declaration.

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
