# Known Issues

This file tracks active engineering issues that are concrete enough to reproduce
or investigate. Keep entries current: remove fixed items after the fix is covered
by tests, and prefer small repros over broad historical failure summaries.

## Active Issues

### Baseline test suite is currently red

- **Impact:** `scripts/test-baseline.sh` cannot be used as a clean pass/fail gate
  until unrelated existing failures are triaged.
- **Observed during latest run:** `Raven.CodeAnalysis.Tests` ended with
  86 failed tests, 2838 passed tests, and 17 skipped tests. Earlier in the same
  baseline run, `Raven.LanguageServer.Tests` also failed to compile due to a
  missing `ObjectInitializerAssignmentEntrySyntax` type.
- **Examples of failing areas:** operator binding, parser pattern shape,
  workspace macro project loading, exception handling diagnostics, match
  expression inference, collection expression parsing, pointer semantics,
  incremental compilation reuse, extension inference, and property codegen.
- **Suggested next step:** Split this into smaller tracked issues by current
  failure cluster. Do not use this broad item as a substitute for focused bugs.

## DSL Follow-ups

These items were found while building the ASP.NET Core trailing-block DSL sample.

### Nested builder blocks with typed function handlers

Using nested builder blocks with a typed function-expression handler exposed an
emission failure:

```text
Missing parameter builder for 'id'
```

The failure appeared with a shape like:

```raven
app.MapDsl {
    Group("/todos") {
        GET("/{id:int}", func (id: int) => store.Describe(id)).Named("todos-get")
    }
}
```

Investigate how function-expression parameters are declared and emitted when the
function expression is nested inside builder-rewritten trailing blocks.

### Delegate and function type consistency

Function type syntax should remain the preferred DSL-facing surface:

```raven
init(pattern: string, handler: T -> string)
```

The compiler should consistently map these signatures to the underlying
`Func<>` or `Action<>` delegate types for method parameters, constructors,
generic inference, overload resolution, and ASP.NET Core interop.

### Constructor and method binding consistency

DSL descriptors often use types as invocable route declarations:

```raven
GET("/{id:int}", func (id: int) => store.Describe(id))
```

Constructor binding and method binding should follow the same argument-binding
rules where practical, including delegate target typing and generic type
argument inference. Same-named non-generic and generic types need predictable
resolution:

- Prefer an applicable non-generic constructor.
- If the non-generic type is not applicable, infer a same-named generic type
  when exactly one candidate succeeds.
- Report ambiguity when multiple generic candidates succeed.

### Identifier trailing blocks inside route groups

A route group would be more natural if Raven could support route-scoped
identifier trailing blocks:

```raven
app.Route("/todos") {
    GET {
        store.Summary()
    }
}
```

This likely requires design work around trailing blocks on identifiers and how a
builder scope supplies contextual route information.

### ASP.NET Core metadata extension overloads

Some ASP.NET Core metadata helpers, such as `WithTags`, can expose generic and
non-generic overloads that Raven currently treats as ambiguous. Investigate
overload specificity and extension-method resolution for these APIs so DSL
metadata helpers can stay thin wrappers over ASP.NET Core.

## Recently Fixed

### Multiple endpoints per route group in the ASP.NET DSL sample

- **Fixed:** The sample has been expanded back to a representative route group:
  `GET("")`, typed `GET("/{id:int}", func ...)`, and `POST("/seed")`.
- **Previous behavior:** This shape was reduced while repeated trailing-block
  emission reused the first endpoint's handler or metadata.
- **Coverage:** The repeated trailing-block codegen coverage above locks the
  compiler behavior. The sample was also runtime-smoke-tested through Kestrel.

### LINQ/lambda lowering in reflection-heavy generic converter code

- **Fixed:** Display-class runtime type construction now preserves constructed
  generic containing types for ordinary nested closure classes, while avoiding
  that reanchoring for extension containers whose receiver generics are lifted
  onto emitted methods.
- **Previous behavior:** LINQ chains such as
  `GetNestedTypes(...).Where(...).ToDictionary(...)` inside generic converter
  code could emit open display-class constructor/delegate references and fail at
  runtime with an uninstantiated-method/type error.
- **Coverage:** `FunctionExpressionCodeGenTests` verifies a generic `Probe<T>`
  captures a local `PropertyInfo` and executes a LINQ
  `Where(...).ToDictionary(...)` chain against nested reflection types.

### Nullable metadata `params` arrays treated collection literals as missing arguments

- **Fixed:** Nullable reference params arrays such as `object?[]?` are unwrapped
  when binding, converting, and emitting collection-literal arguments.
- **Previous behavior:** `Activator.CreateInstance(type, [value])` selected the
  right metadata overload but converted the array argument into an error/default
  value, so emitted IL passed `null` and reflection looked for a parameterless
  constructor.
- **Coverage:** `MethodOverloadTests` verifies the bound argument remains a
  collection expression, and `MethodOverloadCodeGenTests` verifies the reflected
  constructor receives the emitted object array.

### Declared `out var` invocation arguments lost their target type

- **Fixed:** Declared `out var` locals now use the target parameter type when the
  parameter exposes `RefKind.Out` with a plain element type.
- **Previous behavior:** `out var property` for metadata calls such as
  `JsonElement.TryGetProperty("value", out var property)` became an error-typed
  local. Overload resolution then failed quietly and codegen emitted
  `default(bool)` instead of a real call.
- **Coverage:** `ByRefParameterCodeGenTests` covers both source-defined `out`
  methods and metadata `out` methods.
