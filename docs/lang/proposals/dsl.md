# Proposal: Builder Blocks (DSLs)

> ⚠️ 🧩 This proposal has been partly implemented

Raven supports **builder blocks** to enable declarative DSLs that combine
expressions, control flow, and local bindings into a single structured value.

Builder blocks are inspired by Swift’s *result builders* and are designed to be
**type-directed, extensible, and user-definable**.

## Motivation

Builder blocks allow users to write structured, declarative code using normal
language constructs (`if`, `for`, local variables) while deferring interpretation
to a user-defined *builder* type.

Typical use cases include:

* UI composition (SwiftUI-style DSLs)
* Declarative configuration
* Query builders
* Validation pipelines
* Structured code generation

## Overview

A builder block is a `{ ... }` block that is **bound and lowered according to a
builder type**, rather than as a normal block expression, object initializer, or
collection initializer.

The builder type defines how expressions and control-flow constructs inside the
block are combined into a final value.

## Declaring a builder

A builder is declared by defining a type that exposes the appropriate static
builder methods. The type itself does not require special syntax beyond being
referenced by a `[Builder<T>]` attribute.

```raven
class ViewBuilder {
    static BuildBlock(nodes: ViewNode[]) -> ViewNode
    static BuildExpression(view: View) -> ViewNode
    static BuildEither(first: ViewNode) -> ViewNode
    static BuildEither(second: ViewNode) -> ViewNode
    static BuildArray(nodes: ViewNode[]) -> ViewNode
}
```

Builder methods are discovered by name and signature during binding. Missing
methods indicate unsupported language constructs.

## Using a builder

A builder is activated by annotating a function parameter or property with
`[Builder<T>]`:

```raven
func View([Builder<ViewBuilder>] content: () -> ViewNode) -> View {
    ...
}
```

When a `{ ... }` block is supplied for a parameter annotated with `[Builder<T>]`,
the block is treated as a **builder block** and bound using the specified builder.

```raven
View {
    Text("Hello")

    if showImage {
        Image("icon")
    }

    for item in items {
        Row(item)
    }
}
```

## Builder block semantics

A builder block:

* Introduces a normal lexical scope
* Allows local variable declarations
* Allows control-flow statements
* Collects *component expressions* that are rewritten using the builder

Only expressions that participate in the builder are rewritten; other statements
remain normal statements.

## Builder methods

The compiler recognizes the following optional static methods on the builder
type. Their presence determines which constructs are supported.

### `BuildBlock`

```raven
static BuildBlock(components: Component[]) -> Component
```

**Required.** Combines a sequence of components into a single component.

### `BuildExpression`

```raven
static BuildExpression(expr: T) -> Component
```

Optional. Converts a normal expression into a component. If omitted, expressions
must already have the component type.

### `BuildEither`

```raven
static BuildEither(first: Component) -> Component
static BuildEither(second: Component) -> Component
```

Optional. Enables `if / else` expressions inside the builder block.

### `BuildOptional`

```raven
static BuildOptional(component: Component?) -> Component
```

Optional. Enables `if` expressions without an `else`.

### `BuildArray`

```raven
static BuildArray(components: Component[]) -> Component
```

Optional. Enables `for` loops inside the builder block.

### `BuildFinalResult`

```raven
static BuildFinalResult(component: Component) -> TResult
```

Optional. Allows the builder to produce a final result type different from the
component type.

## Control flow in builder blocks

### Conditional expressions

```raven
View {
    if isLoggedIn {
        Text("Welcome")
    } else {
        Text("Please sign in")
    }
}
```

Requires the builder to define `BuildEither`.

### Loops

```raven
View {
    for item in items {
        Row(item)
    }
}
```

Requires the builder to define `BuildArray`.

### Local bindings

```raven
View {
    val title = "Hello"
    Text(title)
}
```

Local bindings behave as normal variables and do not participate directly in
builder rewriting.

## Binding and lowering model

During binding, the compiler:

1. Determines whether a `{ ... }` block is a builder block based on the expected
   target and the presence of a `[Builder<T>]` attribute.
2. Binds the block into a `BoundDslBody` node containing:

   * Bound statements
   * Component-producing expressions
   * An associated builder descriptor
3. Lowers the bound body into calls to the builder’s static methods.

Builder blocks are never inferred implicitly; they are only used when a builder
is explicitly specified.

## Interaction with initializers

Builder blocks are distinct from:

* Object initializers
* Collection initializers
* Normal block expressions

The presence of a `[Builder<T>]` attribute takes precedence over object or
collection initializer binding.

## Diagnostics

The compiler produces targeted diagnostics when:

* A builder block uses an unsupported construct
* Required builder methods are missing
* A component expression cannot be converted by the builder
* Control-flow constructs are used without corresponding builder support

## Future work

* Pattern matching inside builder blocks
* Builder inference based on target type
* Builder composition
* Improved diagnostics for nested builders