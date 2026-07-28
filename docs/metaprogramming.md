# Metaprogramming in Raven

Raven supports several forms of metaprogramming. They operate at different
times and solve different problems:

| Facility | When it runs | Primary purpose |
| --- | --- | --- |
| Procedural macros | During compilation, primarily binding | Transform or introduce Raven syntax with compiler diagnostics and tooling integration |
| .NET reflection | At runtime | Inspect loaded metadata and types, construct objects, and invoke members dynamically |
| Raven compiler APIs | When a host program calls them | Parse and construct syntax, analyze code, build compilations, and emit projects in-process |

These facilities can be combined, but they are not interchangeable.

## Procedural macros

> [!NOTE]
> **Status: work in progress.** Procedural macros are available for
> experimentation, but their syntax, authoring model, compiler contracts, and
> tooling integration remain subject to change.

Procedural macros are Raven's native facility for compile-time metaprogramming.
A freestanding macro looks like a function-style or delimited expression, while
an attached macro is applied as an attribute. The compiler resolves and expands
it during binding, before ordinary code generation.

Use a macro when the metaprogram should affect the program being compiled. A
macro can:

* validate a domain-specific language and report diagnostics at authored source
  locations;
* translate a token stream or typed arguments into ordinary Raven syntax;
* introduce or replace declarations through an attached macro; and
* use `#quote` to construct expansion syntax without assembling every syntax
  node manually.

Because expansion is part of compilation, invalid macro input fails the build
instead of being deferred until the resulting program runs. Macro completion,
navigation, expansion previews, and future structured-DSL tooling are compiler
and editor concerns rather than runtime reflection behavior.

## .NET reflection

Reflection is the .NET runtime facility exposed through APIs such as
`System.Type`, `System.Reflection`, and `Activator`. It examines metadata for
assemblies and types that have been loaded into the running process.

Use reflection when the running application needs to discover or invoke code
dynamically—for example, loading providers, reading attributes, inspecting
members, or constructing a type selected from configuration. Reflection does
not rewrite the Raven compilation that produced the application, and failures
normally occur at runtime.

Reflection can also consume code produced in the same process. A host may use
the Raven compiler APIs to emit an assembly, load it through the .NET assembly
loader, and then inspect or invoke the result through reflection. This is a
combination of compile-time hosting and runtime metaprogramming, not a separate
Raven facility. It trades process startup and interchange costs for in-process
costs such as dependency resolution, retained load contexts, difficult
unloading, executing untrusted code, and failures that can affect the host.

Macros may use ordinary .NET APIs internally, including reflection, but that
does not turn reflection itself into a compiler feature. The macro remains the
compile-time integration point.

## Raven compiler APIs

`Raven.CodeAnalysis` exposes the compiler as an in-process API. A host such as
an editor, build tool, test harness, or code-generation application can:

* parse source into immutable syntax trees;
* inspect and construct syntax tokens and nodes;
* create compilations and add source or metadata references;
* query symbols, types, operations, and diagnostics; and
* emit an assembly from the resulting compilation.

Use the compiler APIs when your program is itself hosting, analyzing, or
driving Raven. Unlike a procedural macro, a compiler API client controls the
compilation from outside the source program being compiled. It may construct
syntax or build an entire project, but it is not automatically invoked because
a macro-like form appears in Raven source. The emitted output may remain an
artifact, run in another process, or be loaded back into the compiler host and
queried with reflection.

### Compile and load in the same process

A compiler host can emit a Raven compilation to an in-memory assembly image,
load that image into the same process with a .NET `AssemblyLoadContext`, and
then use reflection to query or invoke the emitted types. This supports
interactive hosts, test systems, scripting environments, and applications that
need to compile user-provided Raven code without launching a separate compiler
process.

This workflow combines facilities rather than introducing a fourth one:
`Raven.CodeAnalysis` builds and emits the assembly, the .NET loader brings it
into the process, and reflection examines the loaded result. The host remains
responsible for dependency resolution, load-context lifetime and unloading,
trust boundaries, and whether emitted code is allowed to execute. Loading into
the current process is convenient and can avoid a separate-process boundary,
but it also couples the generated code's dependencies, resource consumption,
and failures more closely to the host.

This host-driven workflow is more dynamic than a compile-time macro. The
running application can decide which source and references to compile, when to
load the result, and which emitted types to inspect or execute. A future macro
that compiles code would instead run while the containing program is being
compiled and would be governed by declared build inputs, diagnostics, caching,
and incremental invalidation.

## Future compiler-backed macros

A future macro could expose a controlled compile-and-load workflow during
compilation. That is intentionally outside the procedural-macro MVP. Such a
macro needs compiler-owned rules for nested compilations, project-relative
resources, dependency tracking, incremental invalidation, diagnostics,
assembly-load isolation, caching, and execution policy. An unrestricted
`Assembly.Load` call inside an ordinary macro would not provide a dependable
build or editor experience.

## Choosing a facility

Choose a procedural macro for a reusable compile-time transformation that
belongs in Raven source and should participate in build diagnostics and
tooling. Choose reflection for runtime discovery or dynamic invocation. Choose
the compiler APIs for tools and hosts that need direct control over parsing,
analysis, syntax construction, compilation, emit, or in-process loading of the
emitted result.

## Common scenarios

| Scenario | Facilities | Characteristics |
| --- | --- | --- |
| Validate or transform syntax while building a Raven program | Procedural macro | Runs during compilation, reports build diagnostics, and produces ordinary Raven syntax |
| Inspect types already loaded by the application | Reflection | Runtime-only discovery with no additional compilation or assembly loading |
| Load an existing plugin or provider assembly and discover its types | .NET assembly loading and reflection | Dynamic runtime extensibility; the host owns dependency resolution, trust, and load-context lifetime |
| Parse or analyze Raven source in an editor, test, or build tool | Compiler APIs | The host controls syntax trees, semantic queries, diagnostics, and references without needing to execute the code |
| Generate and emit a Raven assembly as an artifact | Compiler APIs | The output can be saved, referenced by another compilation, or executed elsewhere |
| Compile Raven source, load the emitted assembly into the same process, and query or execute it | Compiler APIs, .NET assembly loading, and reflection | The most dynamic in-process option; convenient for interactive and hosted systems, but couples generated code and its costs to the host |
| Compile code from inside a macro | Future procedural macro plus compiler APIs | A build-time nested compilation rather than runtime dynamism; deliberately deferred until dependency, caching, diagnostics, and execution rules are defined |

The reflection scenarios form a progression. Reflection may inspect metadata
that is already present, it may follow an explicit runtime load of a
precompiled assembly, or it may inspect an assembly that the same process has
just produced with the Raven compiler APIs. Only the last scenario creates new
code dynamically; reflection itself inspects or invokes the resulting .NET
metadata and members.

For compiler hosting, start with the
[compiler API overview](compiler/api/README.md). For project-level extension
points, see [Extend a Raven project](compiler/extending-projects.md).
