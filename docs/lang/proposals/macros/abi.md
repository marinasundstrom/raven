# Macro ABI

Status: **proposed redesign**

This document defines the stable underlying model for Raven macros. The
authored `macro` syntax remains the primary source experience. The redesign is
allowed to break the current category-specific provider interfaces, generated
parameter objects, and adapter layout.

The ABI has two layers:

1. a **declaration ABI** used by binding, symbols, tooling, and generic
   substitution; and
2. an **execution ABI** used to invoke compiled macro providers.

The declaration ABI is intentionally method-shaped. The execution ABI is
intentionally erased because macro type arguments may denote source types that
do not yet exist as loadable CLR types.

## Declaration ABI

Every macro has one nominal definition type and exactly one designated
`Expand` method. A declaration such as:

```raven
macro Awesome<T>(value: T) -> T {
    // ...
}
```

has the canonical semantic shape:

```raven
class AwesomeMacro<T> {
    func Expand(value: T) -> T
}
```

The class-like shape is the compiler model, not replacement source syntax and
not an ordinary runtime type visible to application lookup. The definition
type owns generic parameters and constraints. The designated method owns the
ordered parameter list and return type.

This is analogous to a delegate type and its `Invoke` method: the nominal type
provides stable identity while one callable signature describes application.
Unlike a delegate, a macro executes during compilation and may have parameters
supplied by the compiler.

The public symbol projection should be approximately:

```csharp
public interface IMacroSymbol : ISymbol
{
    INamedTypeSymbol DefinitionType { get; }
    IMethodSymbol ExpandMethod { get; }
    MacroApplicationKind ApplicationKind { get; }
    MacroInvocationTargets InvocationTargets { get; }
    ImmutableArray<MacroParameterBinding> ParameterBindings { get; }
}
```

`IMacroSymbol` remains a macro symbol rather than entering ordinary method or
named-type lookup. `DefinitionType` and `ExpandMethod` are the canonical facts;
convenience properties such as type parameters, parameters, and return type
must project from them rather than own independent copies.

### Generic ownership and construction

Macro generic parameters belong to `DefinitionType`. Parameter and return
types refer to those same symbols. Constructing a macro uses ordinary named
type substitution and obtains the correspondingly substituted `ExpandMethod`.

For example:

```text
AwesomeMacro<T>.Expand(T) -> T
             construct T = int
AwesomeMacro<int>.Expand(int) -> int
```

Macro identity initially consists of canonical name and generic arity. Each
definition has exactly one `Expand` method, so the initial ABI does not require
macro overload resolution. Explicit type arguments and constraints precede
generic inference and overloads in implementation order.

## Parameters and binding sources

`Expand` may declare any number of parameters. Every parameter has one binding
source that states who supplies it:

```csharp
public enum MacroParameterSource
{
    Value,
    SyntaxInput,
    Context,
    TokenBody,
    AttachedTarget,
}
```

The source is compiler-owned metadata on the ordinary `IParameterSymbol`. It
does not create a parallel parameter symbol or a generated parameter-object
property.

```csharp
public sealed class MacroParameterBinding
{
    public IParameterSymbol Parameter { get; }
    public MacroParameterSource Source { get; }
    public int DeclarationOrdinal { get; }
    public int? InvocationArgumentOrdinal { get; }
}
```

Caller-supplied value and syntax inputs have an invocation argument ordinal.
Compiler-supplied context, token-body, and attached-target parameters do not.
All parameters retain declaration order in `ExpandMethod.Parameters`.

For example:

```raven
macro Query<T>(
    dialect: string,
    source: ExpressionSyntax,
    body: IMacroTokenStream,
    context: TokenTreeMacroContext
) -> T {
    // ...
}
```

has four `Expand` parameters but only `dialect` and `source` appear as
method-like invocation arguments. Signature help and completion filter the
canonical parameter bindings rather than reconstructing a second signature.

Application kind and capabilities are derived from parameter sources:

* an `AttachedTarget` parameter selects attached application;
* a `TokenBody` parameter selects a raw-body invocation envelope;
* context parameters request compiler services without changing call syntax;
* otherwise the macro is an argument-style invocation.

Invalid combinations are declaration diagnostics. At most one attached target
and one token body are allowed, an attached target cannot coexist with a token
body, and at most one parameter may request a particular compiler-owned
context capability.

## Execution ABI

The declaration ABI cannot be invoked directly as an ordinary CLR generic
method in every case. A macro type argument may be an `ITypeSymbol` for a type
declared in the consumer compilation, and that type may not have emitted CLR
metadata when expansion runs.

Compiled providers therefore use one erased dispatch contract:

```csharp
public interface IMacroExecutor
{
    MacroExecutionResult Expand(MacroExecutionContext context);
}
```

`MacroExecutionContext` contains one immutable invocation snapshot:

* the resolved macro definition and constructed declaration signature;
* symbolic type arguments as compiler `ITypeSymbol` values;
* bound caller arguments associated with their canonical parameters;
* requested contexts, token body, or attached target;
* the authored carrier and actual application position; and
* lazy compiler services, diagnostics, dependencies, and provenance support.

The compiler lowers a Raven-authored macro body to this executor contract. The
lowering may erase semantic generic types and free-form parameters, but it must
preserve their parameter identities and ordinals in the invocation snapshot.
No generated typed parameter object is part of the stable ABI.

The executor always returns `MacroExecutionResult`, the compiler-owned union
of attached and invocable expansion results. The dispatcher validates that the
selected result matches the invocation application kind.
The authored `ExpandMethod.ReturnType` remains the source-level contract used
for macro application and validation; it is not required to be the physical
CLR return type of the executor.

Class-authored .NET providers use this lower-level execution API directly.
Raven-authored declarations receive the method-shaped declaration experience
and are lowered to the same execution boundary. Provider manifests associate
the exported declaration metadata with its executor entry point.

## Tooling contract

Binding, completion, signature help, hover, documentation, lookup, and
execution consume the same `DefinitionType`, `ExpandMethod`, and parameter
bindings.

The caller-facing signature is a projection, not a second symbol:

* include parameters whose source is `Value` or `SyntaxInput`;
* display token-body syntax separately from parenthesized arguments;
* omit injected context and attached-target parameters from invocation
  argument positions; and
* preserve generic parameters, constraints, defaults, names, and documentation
  from the canonical definition and method.

This ensures a parameter cannot occupy one ordinal in semantic tooling and a
different ordinal during execution.

## Future-compatible requirements

The initial redesign need not implement typed syntax facades, generic
inference, or macro overloads. The ABI must leave room for them.

In particular, a future macro-only `ExpressionSyntax<T>` parameter remains a
`SyntaxInput` whose semantic constraint refers to a definition-type parameter.
The execution ABI must transport its bound syntax and symbolic type information
without constructing `ExpressionSyntax<T>` with a consumer CLR type. No wider
generic syntax-node hierarchy is implied by this requirement.

## Migration direction

The redesign may remove or replace:

* `IAttachedDeclarationMacro<TParameters>`;
* `IInvocableMacro<TParameters>`;
* `ITokenTreeMacro<TParameters>`;
* `IMacroDefinition<TParameters>` and generated parameter objects;
* category-specific `Expand` bridge methods; and
* macro-owned copies of generic parameters, parameters, and return types.

Migration should proceed from compiler-owned semantic facts outward:

1. introduce the nominal definition type and designated `ExpandMethod`;
2. project current macro declarations into that model;
3. move symbol equality, construction, lookup, and tooling to the projection;
4. introduce immutable execution snapshots and the erased executor;
5. lower Raven declarations directly to that executor;
6. migrate compiler-provided macros and class-authored providers; and
7. remove category-specific and parameter-object compatibility APIs.

The current implementation remains authoritative until these slices land.
