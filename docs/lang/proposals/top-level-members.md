# Proposal: Namespace-level functions and constants

> ℹ️ This proposal is under consideration

## Summary

Allow functions and constants to be declared directly as namespace members.
The declaration syntax is top-level within a compilation unit or namespace, but
the source model is that these declarations belong to the namespace rather than
to a user-authored utility type.

```raven
namespace MyApp.Text {
    public func Capitalize(input: string) -> string =>
        input.Length == 0 ? input : char.ToUpper(input[0]) + input[1..]

    const DefaultGreeting: string = "hello"
}

import MyApp.Text.*

func Main() {
    System.Console.WriteLine(Capitalize(DefaultGreeting))
    System.Console.WriteLine(MyApp.Text.Capitalize("raven"))
}
```

The first implementation should support namespace-level functions and
constants. Constants are a reasonable early addition because they are stateless
compile-time values. Fields, properties, events, indexers, and constructors are
out of scope for the initial feature.

This proposal is intentionally close to
[C#'s top-level members proposal](https://github.com/dotnet/csharplang/blob/main/proposals/top-level-members.md)
while keeping Raven's import and namespace syntax.

## Motivation

Raven already supports declaring code at file scope. This feature extends the
namespace surface so reusable APIs can be expressed as functions and constants
owned by a namespace. It removes utility-class boilerplate while preserving a
straightforward .NET metadata shape: the implementation is still emitted as
static members, but Raven source names the namespace, not an invented
container type.

This is useful for:

* small reusable helper APIs,
* namespace-level algorithms,
* extension-adjacent helper functions,
* simple entry points that do not need a `Program` class.

## Goals

* Support namespace-level `func` declarations as static methods.
* Optionally support namespace-level `const` declarations.
* Bring namespace-level functions and constants into unqualified lookup when their namespace is
  wildcard-imported.
* Permit namespace-qualified member access such as `MyApp.Text.Capitalize(...)`.
* Keep normal type declarations as ordinary namespace members even when mixed
  with namespace-level functions and constants.
* Preserve .NET metadata interop by lowering namespace-level functions and constants
  into a generated static class.

## Non-goals

* No top-level fields or mutable namespace state in the initial feature.
* No top-level properties, events, indexers, operators, or constructors in the
  initial feature.
* No user-authored top-level class declaration syntax.
* No special capture of top-level statement locals from namespace-level functions.

## Syntax

Namespace-level functions use normal function declaration syntax at namespace scope:

```raven
namespace MathEx {
    func Square(value: int) -> int => value * value
}
```

Constants use normal constant declaration syntax:

```raven
namespace MathEx {
    public const Tau: double = 6.283185307179586
}
```

Namespace-level functions and constants may appear in block-scoped or file-scoped
namespaces. Types may be mixed with them:

```raven
namespace Geometry

public const OriginName: string = "origin"

public func Distance(a: Point, b: Point) -> double {
    let dx = a.X - b.X
    let dy = a.Y - b.Y
    return Math.Sqrt(dx * dx + dy * dy)
}

public class Point {
    public val X: double { get; }
    public val Y: double { get; }
}
```

## Semantics

These declarations are source-level members of their namespace. They are
emitted into an implicit static container, but that container is a metadata
lowering detail rather than the semantic source owner.

Rules:

* Namespace-level functions are implicitly static.
* Namespace constants are implicitly static metadata constants.
* Namespace-level functions and constants default to `internal` when no accessibility
  modifier is written.
* `public` exposes the member to referencing assemblies.
* `public`, `internal`, and `fileprivate` are the only permitted accessibility
  modifiers.
* `private`, `protected`, `protected internal`, and `private protected` are
  invalid because namespace-level functions and constants have no source-level
  containing type or inheritance surface.
* `static` is invalid on namespace-level functions and constants. They are already
  implicitly static, so the compiler should diagnose the modifier as redundant.
* Namespace-level functions may be overloaded using the same rules as static methods.
* Type declarations in the namespace remain normal type declarations and do not
  become members of the implicit container.
* Namespace-level functions and constants are controlled by the `AllowNamespaceMembers`
  compilation option, named for the top-level declaration syntax. This is
  independent from `AllowGlobalStatements`: disabling top-level statements
  prevents synthesized entry-point statements, but still permits namespace
  `func` and `const` declarations when this option remains enabled.
* Namespace promotion from `[TopLevel]` containers is controlled by
  `AllowNamespaceMemberImports`. Disabling it does not disable namespace-level function
  or constant declarations or metadata emission; it only prevents container
  members from resolving through namespace imports, namespace-qualified access,
  and namespace-member completion.

## Imports and lookup

Raven namespace wildcard imports already use `.*`:

```raven
import MyApp.Text.*

Capitalize("raven")
```

A wildcard namespace import brings accessible namespace-level functions and constants
into unqualified lookup along with the namespace's types. Specific member
imports should also work:

```raven
import MyApp.Text.Capitalize

Capitalize("raven")
```

Qualified access does not require an import:

```raven
MyApp.Text.Capitalize("raven")
```

If a shorter qualifier is desired, a namespace alias can provide it:

```raven
alias Text = MyApp.Text

Text.Capitalize("raven")
```

If a namespace-level function or constant and a type have the same name, normal lookup
context decides which symbol kind is needed. Ambiguous value/member lookup is
diagnosed.

Static types can explicitly opt into this namespace import surface by carrying
the `[TopLevel]` metadata marker:

```raven
namespace MyApp.Text {
    [TopLevel]
    public static class TextHelpers {
        public static func Capitalize(value: string) -> string => value
    }
}

import MyApp.Text.*

Capitalize("raven")
```

This is intended for existing static utility types, including metadata types,
whose static members should be available as namespace-level functions and constants
without exposing the container name in Raven source.

## Qualified access

Raven should not require a user-visible class name to qualify namespace
functions and constants. They appear as part of their namespace, so the
namespace is the source-level qualifier:

```raven
MyApp.Text.Capitalize("raven")
MyApp.Text.DefaultGreeting
```

This is the qualified access rule. Lookup treats `NS.Member` as a namespace
member lookup first, then resolves the implementation to the member emitted on
the namespace's generated container.

The synthesized class remains an implementation detail in Raven source. For
reflection and other .NET languages, the generated type should have a stable
metadata name so public namespace-level functions and constants can be discovered
predictably.

## Metadata lowering

For each namespace that contains namespace-level functions or constants, the compiler
synthesizes one static class in that namespace. Declarations from the same
namespace across multiple files are aggregated into that namespace's container.

Implementation shape:

* The generated class is `public`, `static`, and `partial` in metadata terms.
* Member accessibility controls whether consumers can call individual members.
* The generated class is marked with `System.Runtime.CompilerServices.TopLevelAttribute` (`[TopLevel]`).
* The stable metadata name is `NamespaceMembers` inside the namespace, producing
  a fully qualified metadata identity such as `MyApp.Text.NamespaceMembers`.
* If a source type named `NamespaceMembers` already exists in a namespace that
  also declares namespace-level functions or constants, the compiler reports a metadata name
  conflict.
* Namespace-level functions lower to static methods on the generated class.
* Namespace constants lower to literal static fields on the generated class.

Using a public generated class with internal members by default gives public
members a stable metadata home without accidentally exposing every helper.

## Entry points

A plain top-level `Main` function remains a valid Raven entry point:

```raven
func Main(args: string[]) -> int {
    return 0
}
```

Top-level statements may reuse the same generated container as the implicit
`Main` for the file's namespace or global namespace. If explicit namespace
`Main` and top-level statements both appear in the same compilation,
entry-point resolution should diagnose the conflict rather than selecting one
silently.

The eventual rule should make top-level statement entry points normal entry
point candidates so command-line entry point selection can target them in the
same way as explicit `Main` methods.

## Examples

### Internal helpers by default

```raven
namespace Build

func NormalizePath(path: string) -> string =>
    path.Replace('\\', '/')
```

`NormalizePath` is visible inside the current assembly when `Build.*` is
imported, but is not exposed to consumers.

### Public API

```raven
namespace Build

public func NormalizePath(path: string) -> string =>
    path.Replace('\\', '/')
```

The member is emitted as public and can be consumed from another assembly.

### Qualified disambiguation

```raven
namespace Parsing

public func Parse(text: string) -> Token => ...

namespace Formatting

public func Parse(text: string) -> Format => ...

func Main() {
    let token = Parsing.Parse("id")
    let format = Formatting.Parse("json")
}
```

No container class name is needed to disambiguate the calls.

## Diagnostics

The compiler should report diagnostics for:

* unsupported namespace member declaration kinds,
* invalid modifiers on namespace-level functions and constants,
* redundant `static` modifiers on namespace-level functions and constants,
* conflicts between the synthesized `NamespaceMembers` container and a
  user-declared type with the same metadata name,
* duplicate namespace-level function signatures in the same namespace container,
* ambiguous unqualified lookup from multiple imported namespaces,
* ambiguous qualified lookup when multiple referenced assemblies expose the
  same namespace-qualified function or constant,
* explicit `Main` and top-level statements both providing entry point
  candidates without a command-line disambiguation.

## Implementation notes

The feature affects these compiler areas:

* parser validation for namespace member declarations,
* declaration discovery for namespace-level functions and constants,
* synthesized type creation for each namespace with namespace-level functions or constants,
* import binding and namespace-qualified lookup,
* entry-point discovery,
* semantic APIs such as `GetDeclaredSymbol`, `GetSymbolInfo`, and
  `GetTypeInfo`,
* metadata emit for the generated container and its members,
* language service completion, hover, and go-to-definition.

The public semantic API should expose the source-level namespace-level function or
constant as the declared symbol. It should not require language-service callers
to know whether the answer came from a generated container, cache state, or a
rebind.

## Resolved design decisions

* Namespace-level functions and constants are `internal` by default.
* `public`, `internal`, and `fileprivate` accessibility are allowed.
* The compiler synthesizes one container per namespace that declares namespace
  functions or constants.
* The generated container is marked with `[TopLevel]`.
* Static source or metadata types manually marked with `[TopLevel]` are treated
  as namespace member containers for namespace imports, namespace-qualified
  access, and namespace-member completion.
* The generated container uses the stable metadata name `NamespaceMembers`
  inside its namespace.
* Redundant `static` on namespace-level functions and constants is diagnosed.
