# Macros

## Overview

Raven supports attached declaration macros and freestanding expression macros. A macro is a compiler-driven expansion that produces ordinary Raven syntax before normal semantic analysis continues.

Macros are distinct from .NET attributes:

* `[Serializable]` is an attribute.
* `#[Observable]` is a macro.

Macros are resolved from referenced `RavenMacro` assemblies. Their meaning is defined by the referenced macro implementation, not by the parser.

`MacroKind` remains part of the common `IMacroDefinition` surface, but it is implied by the specialized macro interface:

* `IAttachedDeclarationMacro` implies `AttachedDeclaration`
* `IFreestandingExpressionMacro` implies `FreestandingExpression`

## Attached macro syntax

An attached macro uses a `#` directly followed by an attribute list:

```raven
#[Observable]
var Title: string
```

The `#` token is part of the macro syntax. It is not optional.

### Disambiguation with directives

`#` starts a macro attribute only when it is immediately followed by `[`.

Examples:

```raven
#[Observable]
var Title: string

#pragma warning disable RAV0103
```

`#pragma` and other directive forms remain directives. They do not parse as macros.

## Freestanding macro syntax

A freestanding expression macro uses `#name(...)` in expression position:

```raven
func Main() -> int => #answer()
```

The expression expands to an ordinary Raven expression before normal expression binding continues.

## Placement rules

Macro attributes follow the same placement rules as declaration attributes:

* A macro attribute may appear only directly before a declaration.
* No blank line may separate the macro attribute from the declaration it applies to.
* Multiple attribute lists may appear before the same declaration.
* Normal attributes and macro attributes may be mixed in the declaration prelude.

Example:

```raven
[Obsolete]
#[Observable]
public var Title: string
```

## Arguments

Attached macros may take arguments.

Both positional and named arguments are supported:

```raven
#[Observable]
#[Observable("TitleChanged")]
#[Observable(Name: "TitleChanged", Notify: true)]
```

The compiler parses and preserves these arguments generically. Their interpretation is defined by the macro implementation.

For attached declaration macros, plugins currently receive the raw parsed arguments through `AttachedMacroContext.ArgumentList` and a convenience parsed view through `AttachedMacroContext.Arguments`.

For freestanding expression macros, the equivalent APIs are `FreestandingMacroContext.ArgumentList` and `FreestandingMacroContext.Arguments`.

Each parsed `MacroArgument` exposes a richer constant representation through `Constant`, plus the evaluated CLR value directly through `Value` as a convenience.

This raw-argument model is transitional. The intended direction is typed macro parameter objects, so macro signatures can be validated and presented like normal attributes in completion and signature help. The public contract now includes `IMacroDefinition<TParameters>`, `IAttachedDeclarationMacro<TParameters>`, and `IFreestandingExpressionMacro<TParameters>` for that bound-parameter model.

Example direction:

```csharp
public sealed class ObservableMacroParameters
{
    public bool Notify { get; init; } = true;
    public string? Name { get; init; }
}

public sealed class ObservableMacro : IAttachedDeclarationMacro<ObservableMacroParameters>
{
    ...
}
```

The current typed-parameter binding slice supports:

* one public constructor for positional arguments
* public writable properties for named arguments
* constant conversion into common CLR primitive/reference types

The target experience is that macro arguments bind like attribute arguments:

* completion for named arguments
* signature help for supported shapes
* diagnostics for unknown names, missing required arguments, and invalid constant conversions
* typed parameter access in the macro implementation

## Expansion model

Macro expansion is not a preprocessor step. The source file is parsed normally first. After parsing, the compiler resolves macros from referenced macro assemblies and requests expansions using structured Raven syntax.

The current attached-macro system supports these generic result shapes:

* diagnostics
* introduced members
* replacement of the annotated declaration

Expansion must remain generic. The compiler does not hardcode macro-specific behaviors such as property notification or equality semantics.

Freestanding expression macros return a generic expression-expansion result shape:

* diagnostics
* replacement expression

## Project references

Projects reference macro implementations with `RavenMacro` items in the project file.

Example:

```xml
<ItemGroup>
  <RavenMacro Include="../macros/ObservableMacros.rvnproj" />
</ItemGroup>
```

The compiler loads the referenced macro assembly, resolves exported macros by name, validates target compatibility, and reports failures as ordinary diagnostics.

## Example

```raven
class MyViewModel: ObservableBase {
    #[Observable]
    var Title: string
}
```

In this example, `#[Observable]` is an attached property macro. The macro may replace the property declaration with ordinary Raven members such as backing storage and accessor bodies.
