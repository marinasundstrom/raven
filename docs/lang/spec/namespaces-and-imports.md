# Namespaces, imports, and aliases

Namespaces organize types and functions and prevent names from colliding.
Imports make those names shorter to use, while aliases provide a different
local name when that improves clarity or resolves a conflict.

Each file may define a namespace:

```raven
namespace Foo

// Members here
```

### Import directive

```raven
namespace Foo

import System.*
// or import System.Collections.*

// Members here
```

The wildcard may also be applied to a type name to bring its static members
and nested types into scope:

```raven
import System.Math.*

val pi = PI
```

Enum members are constants and participate in type-member imports. A wildcard
type import such as `import System.AttributeTargets.*` brings all enum members
into unqualified value scope, and an individual import such as
`import System.AttributeTargets.Delegate` brings that single member into scope.
An individual enum-member import is deliberate and follows the existing
precedence rules for specific imports.

Extension methods defined on imported types are also brought into scope. This
enables consuming .NET helpers such as `System.Linq.Enumerable.Where` or
`System.MemoryExtensions.AsSpan` directly from Raven source:

```raven
import System.Collections.Generic.*
import System.Linq.*

val odds = List<int>()
odds.Add(1)
odds.Add(3)
val filtered = odds.Where(value => value % 2 == 1)
```

The compiler treats `Where` as an instance-style invocation even though it is
declared as a static C# extension method, inserting the receiver as the first
argument when emitting IL.

Import directives appear at the beginning of a compilation unit or namespace and
simply make existing namespaces or types available. They do not introduce new
names. To bind a custom name, use an `alias` directive. All imports for a given
scope must come before any alias directives or member declarations. Placing an
import directive after an alias or member in the same scope is a compile-time
error (`RAV1005`).

An import belongs to the scope that syntactically contains it. Compilation-unit
imports are visible to declarations in that compilation unit, including
declarations inside a file-scoped namespace. Imports inside a block-scoped
namespace are visible only inside that namespace declaration and its nested
declarations; they are not visible to sibling namespace declarations.
File-scoped namespace imports participate in the same file namespace context as
compilation-unit imports. For clarity, style guidance recommends placing imports
and aliases after the file-scoped namespace declaration, but imports before the
file-scoped namespace declaration are also part of the file's namespace context.

Project builds also include a generated prelude source file. The prelude uses a
top-level `global` block. Global blocks are top-level constructs whose supported
`import` entries are processed early during binding as compilation-wide imports:

```raven
global {
    import System.*
    import System.Collections.*
    import System.Collections.Generic.*
    import System.IO.*
    import System.Linq.*
    import System.Net.Http.*
    import System.Threading.*
    import System.Threading.Tasks.*
    import System.Result.*
    import System.Option.*
}
```

The imports in this generated file participate in ordinary import lookup for
the project. Global imports are hoisted, but they still use ordinary namespace
and type import binding rules. Namespace imports are the preferred project-file
shape. Type-scope imports such as `System.Result.*` and direct nested-case
imports such as `System.Result.Ok` require the imported type or nested type to
be available to the compilation; they are supported, but are best reserved for
stable library/prelude cases.

The standard `Result` and `Option` case imports make `Ok`, `Error`,
`Some`, and `None` available by simple name in project code. User-defined union
cases are not automatically available as simple names; use a qualified case
name, target-typed member syntax, or an explicit wildcard type import such as
`import FridgeError.*`.

Project files can disable the generated standard imports with
`GeneratePreludeImports=false`. They can also add generated prelude imports with
MSBuild `Import` items, including static type-scope imports and generated
aliases:

```xml
<ItemGroup>
  <Import Include="SuperheroApp.Models" />
  <Import Include="System.Console" Static="True" />
  <Import Include="System.DateTime" Alias="DT" />
</ItemGroup>
```

The first two items generate import entries in the global prelude block; the
alias item generates a project-wide alias from the generated prelude file.
Repeating an import that is already supplied globally is redundant and may be
reported as a hidden diagnostic with an editor fix to remove the local import.

### Alias directive

The `alias` directive assigns an alternative name to a fully qualified
**namespace**, type, static member, or to type expressions such as tuples.

```raven
alias IO = System.IO
alias SB = System.Text.StringBuilder
alias PrintLine = System.Console.WriteLine
alias Pair = (x: int, y: int)
alias Flag = bool
alias Text = string

val sb = SB()
PrintLine("Hi")
val tmp = IO.Path.GetTempPath()
```

Aliasing a method binds a specific overload. Multiple directives using the
same alias name may appear to alias additional overloads, forming an overload
set.

Predefined types may be aliased directly. The supported built-in alias targets
are `bool`, `char`, `int`, `long`, `float`, `double`, `string`, and `unit`
(spelled `unit` or `()`). Raven has no `void`; the `unit` type is used instead
(see [implementation notes](dotnet-implementation.md#unit-type)). If the alias
target is invalid, the compiler emits diagnostic `RAV2020`.

Aliases require fully qualified names for namespaces, types, and members to
avoid ambiguity; type expressions are written directly. Alias directives may
appear at the top of a file or inside a namespace but must follow all import
directives and precede any declarations or statements. An alias directive that
appears after a member declaration produces diagnostic `RAV1006`.

### Scoped namespaces

You may define multiple namespaces (including nested) in one file using
block scopes:

```raven
// Members in the global namespace

namespace A1 {
    import System.*
    import System.IO.*

    // Members here

    namespace B1 {
        // Members here
    }
}

namespace A.B {
    // Members here
}
```

The outermost undeclared namespace is the **global namespace**.
