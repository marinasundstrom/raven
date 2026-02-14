## Lowering Sketch

This section describes how builder blocks are represented in the bound tree and how they are lowered into builder calls (and, for UI DSLs, into a framework-agnostic node specification model).

### Goals

* Keep syntax **normal Raven** (`if`, `for`, locals, expressions).
* Make DSL participation **explicit** (only via `[Builder<T>]`).
* Lowering is **deterministic** and driven by builder + adapter capabilities.
* **No positional args** for DSL node construction; named args become property/event assignments.

---

## 1) Bound representation

When a `{ ... }` block is used in a builder context, it binds to a dedicated body node rather than a normal `BoundBlockExpression`.

### `BoundDslBody`

Conceptually:

* `Statements`: bound statements that remain “normal” (locals, assignments, etc.)
* `Components`: expressions that contribute to the builder result
* `Builder`: resolved builder descriptor (methods + component/result types)
* `Adapter`: optional runtime adapter descriptor (UI/stateful DSLs)

A practical shape:

```text
BoundDslBody
  Builder: BuilderDescriptor
  ComponentType: TypeSymbol
  ResultType: TypeSymbol
  Items: BoundDslItem[]
```

Where each item preserves original ordering:

```text
BoundDslItem :=
  - BoundDslStatementItem(Statement)
  - BoundDslComponentItem(ComponentExpr)
```

This matters because DSL bodies intermix statements and component expressions.

---

## 2) Component expressions inside DSL bodies

A “component expression” is any expression that participates in builder rewriting.

There are two important cases:

1. **Normal expression component**
   Example: `Label(Text: "Hello")` (when it already returns `ComponentType`), or `someExpr` that can be wrapped by `BuildExpression`.

2. **Node construction component** (UI/stateful DSLs)
   Example: `StackLayout(Orientation: .Horizontal) { ... }`
   This binds as a node-spec construction that the adapter can interpret.

### `BoundDslComponent`

```text
BoundDslComponent
  Expression: BoundExpression
  Kind: Normal | NodeSpec
```

---

## 3) Binding node construction (named args only)

Inside a DSL body, an invocation expression is eligible for **node construction binding** when:

* The callee resolves to a type or factory symbol that is recognized by the adapter as a node kind (e.g. MAUI control types), and
* All arguments are **named**.

#### Example

```raven
StackLayout(Orientation: .Horizontal, Spacing: 8)
```

Binds into:

```text
BoundDslNodeCreation
  NodeType: TypeSymbol  // Microsoft.Maui.Controls.StackLayout
  Assignments: BoundDslAssignment[]  // Orientation=..., Spacing=...
  Key: optional BoundExpression       // from reserved metadata (Key:)
  Body: optional BoundDslBody         // trailing block, if any
```

Where:

```text
BoundDslAssignment
  Name: string
  Value: BoundExpression
  TargetKind: Property | Event | Metadata
```

### Reserved metadata

The binder recognizes some names as DSL metadata, not framework properties:

* `Key: <expr>` → `TargetKind = Metadata`

Metadata assignments are stored on the node creation and are not applied as framework properties.

### Diagnostics

* Any positional argument inside node creation in a DSL body is an error:

  * “Positional arguments are not allowed in DSL node construction; use named arguments.”
* Duplicate property/event names are errors.
* If the adapter can’t resolve a property/event name, error (or defer to runtime with a warning, depending on policy).

---

## 4) Lowering overview: from `BoundDslBody` to builder calls

Lowering produces a single `BoundExpression` of the builder’s result type.

Lowering is performed in two stages:

1. **Lower each DSL component into a `ComponentType` expression**
2. **Combine all components via `BuildBlock`**
3. **Optionally wrap via `BuildFinalResult`**

---

## 5) Lowering rules

### 5.1 Lowering a DSL body

Given:

```text
BoundDslBody(items=[...])
```

Lower to:

```text
components = LowerItemsToComponentExpressions(items)
block = Call Builder.BuildBlock(components[])
result =
  if Builder has BuildFinalResult:
      Call Builder.BuildFinalResult(block)
  else:
      block
```

`BuildBlock` is required.

Statements in the DSL body are emitted as normal bound statements in the surrounding lowered form; only component items contribute to `components[]`.

---

### 5.2 Lowering a normal component expression

If the component expression’s type is already `ComponentType`, use it directly.

Else, if builder has `BuildExpression(T -> ComponentType)`, lower to:

```text
Call Builder.BuildExpression(expr)
```

Otherwise, diagnostic:

* “Expression of type X cannot be used in this builder block; missing BuildExpression.”

---

### 5.3 Lowering `if` / `else`

If the builder supports `BuildEither`, then:

```raven
if cond { A } else { B }
```

lowers to:

```text
tmp = if cond
        Call Builder.BuildEither(first: LowerBody(A))
      else
        Call Builder.BuildEither(second: LowerBody(B))
```

Where `LowerBody(A)` returns a `ComponentType` (the body’s `BuildBlock` result).

If builder supports `BuildOptional` and there is no `else`:

```raven
if cond { A }
```

lowers to:

```text
tmp = if cond
        LowerBody(A)
      else
        default(ComponentType?)   // or null if nullable
tmp2 = Call Builder.BuildOptional(tmp)
```

(Exact nullability depends on your ComponentType strategy.)

If required methods are missing → diagnostic.

---

### 5.4 Lowering `for` loops

If builder supports `BuildArray`, then:

```raven
for item in items { Body }
```

lowers to:

```text
arr = new List<ComponentType>()
for item in items:
    arr.Add(LowerBody(Body))
tmp = Call Builder.BuildArray(arr.ToArray())
```

This `tmp` is a single `ComponentType` that represents the loop output.

If `BuildArray` missing → diagnostic.

---

## 6) Lowering node construction components (UI/stateful DSLs)

Node construction yields a `ComponentType` by creating a **node spec** expression that the adapter can interpret.

This keeps framework integration out of the compiler and in the runtime adapter.

### 6.1 Node spec construction model

Lower:

```text
BoundDslNodeCreation(NodeType=T, Assignments=[...], Body=optional)
```

to:

```text
spec = Call DslRuntime.CreateNode(typeToken: T)
for each assignment:
    if TargetKind == Property:
        spec = Call DslRuntime.WithProperty(spec, name, value)
    if TargetKind == Event:
        spec = Call DslRuntime.WithEvent(spec, name, value)
    if TargetKind == Metadata (Key):
        spec = Call DslRuntime.WithKey(spec, value)

if Body exists:
    children = LowerDslBodyToChildrenArray(Body)
    spec = Call DslRuntime.WithChildren(spec, children)

component =
  if ComponentType == NodeSpecType:
      spec
  else:
      // optional: allow builder BuildExpression(NodeSpec -> Component)
      Call Builder.BuildExpression(spec) or diagnostic
```

Notes:

* `CreateNode/WithProperty/WithChildren` are not language features; they are ordinary runtime helpers in a DSL runtime library.
* The adapter interprets these specs during reconciliation.

### 6.2 Children lowering

The trailing `{ ... }` of a node is itself a DSL body. The easiest runtime contract is:

* Node children are an **array of node specs/components** (not a single combined `BuildBlock` component).

So for node-body lowering, use a *children lowering mode*:

```text
LowerDslBodyToChildrenArray(body):
    // collect only component items (statements allowed but ignored as children)
    return [ LowerComponentItemToComponentType(item) ... ]
```

This is distinct from `BuildBlock`, which collapses components into one.

This difference is important because UI frameworks need ordered child lists.

(If you prefer a single representation: let `BuildBlock` return something list-like, but it tends to complicate builders that aren’t UI.)

---

## 7) Two lowering modes: `Component` vs `Children`

To support both “builder result” and “node children”, define two related lowering entry points:

* `LowerToComponent(body) -> ComponentType`
  Uses `BuildBlock` (+ optional `BuildFinalResult` at top-level only).

* `LowerToChildren(body) -> ComponentType[]`
  Extracts each component expression as an element (preserving order), without calling `BuildBlock`.

`LowerToComponent` is used for:

* top-level builder argument bodies
* `if` arm bodies
* loop bodies (per-iteration body)

`LowerToChildren` is used for:

* trailing blocks attached to node construction

This mirrors what you want at runtime: “a node has children” rather than “a node has one built block value”.

---

## 8) Example lowering trace

Source:

```raven
StackLayout(Orientation: .Vertical) {
    if showHeader {
        Label(Text: "Header")
    }

    for item in items {
        Label(Text: item.Name, Key: item.Id)
    }
}
```

Bound:

* `BoundDslNodeCreation(StackLayout, Assignments=[Orientation=.Vertical], Body=BoundDslBody([...]))`
* Nested `if` and `for` bind as DSL constructs within the child body.

Lower (conceptual):

* `spec = CreateNode(StackLayout)`
* `spec = WithProperty(spec, "Orientation", Vertical)`
* `children = LowerToChildren(body)`

  * child0 = lower `if` to a `ComponentType` (requires `BuildOptional` or `BuildEither`)
  * child1 = lower `for` to a `ComponentType` (requires `BuildArray`)
* `spec = WithChildren(spec, children)`
* return `spec`

At runtime, the adapter sees:

* Node type: `StackLayout`
* props: Orientation
* children: an ordered list of child node specs/components
* keys on children for stable reconciliation