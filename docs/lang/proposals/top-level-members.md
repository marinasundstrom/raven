# Proposal: Top-level members

> ℹ️ This proposal is under consideration

## Summary

Allow selected members to be declared directly in a namespace and make those
members available when that namespace is wildcard-imported.

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

The first implementation should support top-level methods. Top-level constants
are a reasonable early addition because they are stateless compile-time values.
Fields, properties, events, indexers, and constructors are out of scope for the
initial feature.

This proposal is intentionally close to
[C#'s top-level members proposal](https://github.com/dotnet/csharplang/blob/main/proposals/top-level-members.md)
while keeping Raven's import and namespace syntax.

## Motivation

Raven already supports plain top-level functions, including a top-level
`Main`. Extending that model into namespaces removes utility-class boilerplate
without changing the semantic model: top-level members are still static members,
but the source does not force authors to invent a container type.

This is useful for:

* small reusable helper APIs,
* namespace-scoped algorithms,
* extension-adjacent helper functions,
* simple entry points that do not need a `Program` class.

## Goals

* Support namespace-level `func` declarations as static methods.
* Optionally support namespace-level `const` declarations.
* Bring top-level members into unqualified lookup when their namespace is
  wildcard-imported.
* Permit namespace-qualified member access such as `MyApp.Text.Capitalize(...)`.
* Keep normal type declarations as ordinary namespace members even when mixed
  with top-level members.
* Preserve .NET metadata interop by lowering top-level members into a generated
  static class.

## Non-goals

* No top-level fields or mutable namespace state in the initial feature.
* No top-level properties, events, indexers, operators, or constructors in the
  initial feature.
* No user-authored top-level class declaration syntax.
* No special capture of top-level statement locals from top-level methods.

## Syntax

Top-level methods use normal function declaration syntax:

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

Top-level members may appear in block-scoped or file-scoped namespaces. Types
may be mixed with top-level members:

```raven
namespace Geometry

public const OriginName: string = "origin"

public func Distance(a: Point, b: Point) -> double {
    val dx = a.X - b.X
    val dy = a.Y - b.Y
    return Math.Sqrt(dx * dx + dy * dy)
}

public class Point {
    public val X: double { get; }
    public val Y: double { get; }
}
```

## Semantics

Top-level members are source-level members of their namespace. They are emitted
into an implicit static container, but that container is a metadata lowering
detail rather than the semantic source owner.

Rules:

* Top-level methods are implicitly static.
* Top-level constants are implicitly static metadata constants.
* Top-level members default to `internal` when no accessibility modifier is
  written.
* `public` exposes the member to referencing assemblies.
* `public` and `internal` are the only permitted accessibility modifiers.
* `private`, `protected`, `protected internal`, and `private protected` are
  invalid because top-level members have no source-level containing type or
  inheritance surface.
* `static` is invalid on top-level members. The members are already implicitly
  static, so the compiler should diagnose the modifier as redundant.
* Top-level members may be overloaded using the same rules as static methods.
* Type declarations in the namespace remain normal type declarations and do not
  become members of the implicit container.

## Imports and lookup

Raven namespace wildcard imports already use `.*`:

```raven
import MyApp.Text.*

Capitalize("raven")
```

A wildcard namespace import brings accessible top-level members from that
namespace into unqualified lookup along with the namespace's types. Specific
member imports should also work:

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

If a top-level member and a type have the same name, normal lookup context
decides which symbol kind is needed. Ambiguous value/member lookup is diagnosed.

## Qualified access

Raven should not require a user-visible class name to qualify top-level members.
Top-level members appear as part of their namespace, so the namespace is the
source-level qualifier:

```raven
MyApp.Text.Capitalize("raven")
MyApp.Text.DefaultGreeting
```

This is the qualified access rule. Lookup treats `NS.Member` as a namespace
member lookup first, then resolves the implementation to the member emitted on
the namespace's top-level container.

The synthesized class remains an implementation detail in Raven source. For
reflection and other .NET languages, the generated type should have a stable
metadata name so public top-level members can be discovered predictably.

## Metadata lowering

For each namespace that contains top-level members, the compiler synthesizes one
static class in that namespace. Top-level members from the same namespace across
multiple files are aggregated into that namespace's container.

Implementation shape:

* The generated class is `public`, `static`, and `partial` in metadata terms.
* Member accessibility controls whether consumers can call individual members.
* The generated class is marked with `[TopLevel]`.
* The stable metadata name is `TopLevelMembers` inside the namespace, producing
  a fully qualified metadata identity such as `MyApp.Text.TopLevelMembers`.
* If a source type named `TopLevelMembers` already exists in a namespace that
  also declares top-level members, the compiler reports a metadata name
  conflict.
* Top-level methods lower to static methods on the generated class.
* Top-level constants lower to literal static fields on the generated class.

Using a public generated class with internal members by default gives public
members a stable metadata home without accidentally exposing every helper.

## Entry points

A plain top-level `Main` function remains a valid Raven entry point:

```raven
func Main(args: string[]) -> int {
    return 0
}
```

Top-level statements may reuse the same generated top-level container as the
implicit `Main` for the file's namespace or global namespace. If explicit
top-level `Main` and top-level statements both appear in the same compilation,
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
    val token = Parsing.Parse("id")
    val format = Formatting.Parse("json")
}
```

No container class name is needed to disambiguate the calls.

## Diagnostics

The compiler should report diagnostics for:

* unsupported top-level member kinds,
* invalid modifiers on top-level members,
* redundant `static` modifiers on top-level members,
* conflicts between the synthesized `TopLevelMembers` container and a
  user-declared type with the same metadata name,
* duplicate top-level member signatures in the same namespace container,
* ambiguous unqualified lookup from multiple imported namespaces,
* ambiguous qualified lookup when multiple referenced assemblies expose the
  same namespace-qualified top-level member,
* explicit `Main` and top-level statements both providing entry point
  candidates without a command-line disambiguation.

## Implementation notes

The feature affects these compiler areas:

* parser validation for namespace member declarations,
* declaration discovery for namespace-scoped functions and constants,
* synthesized type creation for each namespace with top-level members,
* import binding and namespace-qualified lookup,
* entry-point discovery,
* semantic APIs such as `GetDeclaredSymbol`, `GetSymbolInfo`, and
  `GetTypeInfo`,
* metadata emit for the generated container and its members,
* language service completion, hover, and go-to-definition.

The public semantic API should expose the source-level top-level member as the
declared symbol. It should not require language-service callers to know whether
the answer came from a generated container, cache state, or a rebind.

## Resolved design decisions

* Top-level members are `internal` by default.
* Only `public` and `internal` accessibility are allowed.
* The compiler synthesizes one top-level container per namespace.
* The generated container is marked with `[TopLevel]`.
* The generated container uses the stable metadata name `TopLevelMembers`
  inside its namespace.
* Redundant `static` on top-level members is diagnosed.
