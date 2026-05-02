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

## Recently Fixed

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
