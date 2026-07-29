# Raven Macro Library

`Raven.Macros` is the standard compiler-plugin library distributed with Raven.
It contains the conventional `quote` and `compile` macros without making them
intrinsic compiler declarations or members of `Raven.Core`.

Applications opt into the short aliases by importing the macro namespace:

```raven
import Raven.Macros.*

let syntax = quote! {
    left + right
}
```

The canonical names `Raven.Macros.Quote!` and `Raven.Macros.Compile!` remain
available when an alias is shadowed or the wildcard namespace is not imported.
Merely referencing the standard library does not place its aliases in lexical
scope.

## Authoring model

The project under `src/Raven.Macros` is written in Raven. Each public macro has
its own source file, uses `macro func`, and carries Markdown documentation
comments. `AssemblyInfo.rvn` marks the output as a compiler plugin:

```raven
[assembly: RavenCompilerPlugin]
```

When a marked Raven library contains macro-function declarations, emission
lowers those declarations into reusable provider types and includes them in the
plugin assembly. This is the same general assembly-plugin mechanism available
to other Raven macro libraries; the standard macros do not require a built-in
macro category.

## Dependency and compatibility model

Macro implementations program against `Raven.CodeAnalysis`, so a macro library
has an ordinary dependency on a particular compiler-contract version. The
compiler loads the macro provider with its dependency closure and the consuming
compilation receives metadata references from that closure. Runtime
dependencies are copied only when the emitted application actually references
them.

Macro authors are responsible for building against a `Raven.CodeAnalysis`
version compatible with the compiler host that loads the plugin. The loader
must reject incompatible contracts with a diagnostic rather than allowing a
late type-load or invocation failure.

## Current implementation boundary

`Quote.rvn` and `Compile.rvn` own the public macro declarations, namespace,
aliases, and documentation. Their low-level expansion mechanics currently
delegate to `StandardMacroExpansions` in `Raven.CodeAnalysis`. This is
intentional while the macro-function API is still evolving.

As the public API gains sufficient diagnostic, source-location, and
syntax-construction support, suitable behavior can move wholly or partly into
the Raven-authored macro project. The present boundary should not be treated as
the final architecture.

## Documentation publishing

The Raven.Macros project emits both XML documentation and Markdown documentation
artifacts. The future documentation site should publish Raven.Macros alongside
Raven.Core so the standard runtime library and standard compile-time library
have parallel API documentation.
