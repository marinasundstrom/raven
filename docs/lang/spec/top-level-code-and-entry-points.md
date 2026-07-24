# Top-level code and entry points

Top-level code keeps small programs and shared functions concise by avoiding a
container class that exists only for structure. Entry points define where an
executable program starts.

A compilation unit or namespace may declare top-level `func` and `const`
declarations at namespace scope. These declarations are namespace members in
source, not members of a user-authored type:

```raven
namespace Utilities {
    public const Answer: int = 41

    public func AddOne(value: int) -> int => value + 1
}
```

Top-level functions and constants are implicitly static. They are emitted into a
synthesized static container named `NamespaceMembers` in the containing
namespace, but that container is a metadata detail. Source lookup treats the
declarations as namespace members. A wildcard namespace import brings accessible
top-level functions and constants into unqualified
lookup, and namespace-qualified member access can reference them:

```raven
import Utilities.*

val a = AddOne(Answer)
val b = Utilities.AddOne(Utilities.Answer)
```

The synthesized container is emitted with the metadata marker attribute
`System.Runtime.CompilerServices.TopLevelAttribute`. A user-authored static class or struct marked with
`[TopLevel]` is also a namespace-member container: its accessible static
members are promoted through the containing namespace for wildcard imports,
specific imports, namespace-qualified access, and namespace-member completion.
This allows existing static utility types to opt into namespace-shaped access
without changing their metadata member ownership.

The effect at a call site is intentionally similar to importing static members
from a utility type, such as `import Foo.UtilityClass.*`. The semantic model is
distinct: top-level functions and constants are declared in and imported from
the namespace itself, so authors can place `func` and `const` declarations in
any file and import them by namespace without inventing a containing utility
class.

Top-level functions and constants default to `internal`. `public`, `internal`,
and `fileprivate` are valid accessibility modifiers. `fileprivate` restricts
access to the declaring source file. `static` is invalid on a top-level function
or constant because the declaration is already implicitly static; the compiler
diagnoses the modifier rather than treating it as meaningful. `private`,
`protected`, `protected internal`, and `private protected` are invalid because a
top-level function or constant has no source-level containing type or
inheritance surface.

Top-level `func` declarations support the same parameter, return type, generic
type parameter, and `where` constraint syntax as methods and local functions.
Top-level `const` declarations follow constant binding rules and are emitted as
metadata literal fields on the synthesized container. Class, struct, interface,
enum, union, delegate, and extension declarations remain normal declarations;
they are not moved into the synthesized `NamespaceMembers` container, and type
declarations such as classes and structs may still use their normal explicit
`static` modifier where that modifier is valid.

Top-level functions and constants are controlled by the `AllowNamespaceMembers`
compilation option. This option is independent of `AllowGlobalStatements`:
disabling top-level statements prevents synthesized entry-point code from
file-scope statements, but top-level `func` and `const` declarations remain
valid when namespace members are enabled. Namespace promotion from
namespace-member containers is controlled separately by
`AllowNamespaceMemberImports`; disabling that option leaves the declarations and
metadata containers intact but prevents container members from being imported or
offered as namespace members.

Top-level functions do not capture locals declared by file-scope statements. A
top-level function body is a static function body, so it can only reference
parameters, imported symbols, and accessible namespace/type members.

## Entry points

An entry point is the code the host runs when a console application starts.

### Supported entry-point forms

Console applications may start in any of the following shapes, all of which obey
the same signature rules:

* **File-scope code (global statements):** executable statements at the top of a
  file. These coexist with other declarations (namespaces, types, top-level
  functions and constants) in the same compilation unit.
* **Top-level function:** a namespace-scope `func Main` declaration that can
  appear next to other top-level functions and constants. When present, no
  other file-scope statements may appear alongside it.
* **Classic static method:** a `Main` method declared on a type such as
  `Program.Main`.

### File-scope code rules

Files may start with executable statements that aren't enclosed in a function or
type. This file-scope code forms the application's entry point and is translated
into a synchronous `Program.Main` plus an async `Program.MainAsync` that returns
`Task` or `Task<int>` depending on whether the script returns a value. Only
console applications may include file-scope code, and
it may appear in at most one file per compilation. When present, these
statements execute in source order. Top-level type declarations are hoisted for
binding, so helper types may appear anywhere in the file or its file-scoped
namespace without changing the execution order of file-scope code.

Top-level functions are hoisted and may be referenced from anywhere in their
namespace scope, regardless of source order. When a file contains *only*
top-level functions and constants, the compiler skips synthesizing the implicit
`Program.Main` bridge; entry-point discovery falls back to user-defined
candidates such as a namespace-scope `func Main` alongside other declarations.

Function and block bodies may also declare local helper `class`, `struct`,
`record`, and `enum` types. These declarations are scoped to the containing
body and exist to encapsulate types that are only used locally. Within a given
body, local type declarations are hoisted for binding like local functions, so
their source order does not affect name lookup inside that body. The compiler
emits them as compiler-mangled nested types under the enclosing containing
type, so they do not publish a stable source-facing outer type name.

Defining a top-level `func Main` suppresses additional file-scope statements.
Any other file-scope statement (including variable declarations or
expressions) in the same compilation unit causes the compiler to emit
`RAV1021` *Top-level statements are not allowed when 'Main' is declared as a
top-level function*.

### Entry point resolution

Console applications begin executing at the synthesized `Program.Main` bridge
that forwards to the async `Program.MainAsync` backing file-scope code. When a
project does not contain runnable file-scope statements, the compiler instead
looks for a user-defined entry point. Any
static method named `Main` qualifies when it meets the following requirements:

* The method returns one of:
  `unit`, `int`, `Task`, `Task<int>`, `Result<int, E>`, `Result<(), E>`,
  `Task<Result<int, E>>`, or `Task<Result<(), E>>`.
* It has no type parameters.
* It declares either no parameters or a single parameter of type `string[]`
  (representing the command-line arguments).

If exactly one method satisfies these conditions, it becomes the entry point for
the compilation. When no method qualifies, the compiler reports
`RAV1014` *Entry point 'Main' not found*. Declaring more than one valid
`Main` (including mixing top-level statements with a matching method) causes the
compiler to emit `RAV1017` *Program has more than one entry point defined*.

When the selected entry point returns `Task`, `Task<int>`, `Result<int, E>`,
`Result<(), E>`, `Task<Result<int, E>>`, or `Task<Result<(), E>>`, the compiler emits
an implicit synchronous bridge method in the entry point's containing type. For
file-scope code the bridge is `Program.Main`, while user-defined async entries
receive a synthesized `<Main>_EntryPoint` neighbor unless a synchronous `Main`
already exists. The bridge calls into the async implementation, awaits it via
`GetAwaiter().GetResult()`, and forwards the resulting value (if any) to the host
environment. A `Task`-returning entry point produces a bridge whose CLR
signature omits a return value; the helper awaits the async body, discards the
awaited `Unit` value, and only returns after the async work (such as console writes)
completes. Exceptions thrown from the async body bubble through the same
`GetResult()` call so the process exits with the same failure semantics as a
purely synchronous entry point. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L352-L403】【F:test/Raven.CodeAnalysis.Tests/CodeGen/CodeGeneratorTests.cs†L88-L144】

Entry points that return `Task<int>` produce a bridge that awaits the async body
and returns the awaited integer as the process exit code. Entry points that
return `Result<int, E>` or `Task<Result<int, E>>` use `Ok(value)` as the process
exit code. Entry points that return `Result<(), E>` or `Task<Result<(), E>>`
produce no output on `Ok`, while `Error` payload data is printed to standard
error.
The bridge also leaves console writes intact so the awaited value can be
observed by both the caller and the host operating system.
【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L405-L476】

Library and script output kinds ignore the entry point search; they never report
missing or ambiguous entry-point diagnostics.
