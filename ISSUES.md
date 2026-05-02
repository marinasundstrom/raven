# Known Issues

This file tracks active engineering issues that are concrete enough to reproduce
or investigate. Keep entries current: remove fixed items after the fix is covered
by tests, and prefer small repros over broad historical failure summaries.

## Active Issues

### Reflection overload resolution with object-array arguments

- **Impact:** Calls such as `Activator.CreateInstance(type, [caseValue])` or
  `Activator.CreateInstance(type, args)` can fail overload resolution or bind to
  the wrong shape from Raven source. The universal union JSON converter currently
  avoids this by using `ConstructorInfo.Invoke(args.ToArray())`.
- **Repro sketch:**

  ```rav
  import System.*

  class Program {
      public static func Run(type: Type, value: object) -> object {
          Activator.CreateInstance(type, [value])
              ?? throw InvalidOperationException("missing")
      }
  }
  ```

- **Expected:** The collection literal should target `object?[]`/`object[]` and
  select the `Activator.CreateInstance(Type, object?[]?)` overload.
- **Observed:** During the converter investigation this shape either failed
  overload resolution or behaved as if no constructor arguments were passed.
- **Likely area:** Invocation argument target typing, collection literal
  conversion, nullable array element compatibility, and overload selection for
  metadata `params`/array parameters.
- **Suggested coverage:** Add a focused codegen test that invokes a known
  constructor through `Activator.CreateInstance(Type, object[])` and verifies the
  constructed value.

### LINQ/lambda lowering in reflection-heavy generic converter code

- **Impact:** The original universal union converter used LINQ chains such as
  `GetNestedTypes(...).Where(...).ToDictionary(...)`. The generated output had
  broken closure/delegate shapes in this context, so the converter was rewritten
  with simple loops.
- **Repro sketch:**

  ```rav
  import System.*
  import System.Collections.Generic.*
  import System.Linq.*
  import System.Reflection.*

  class Probe<T> {
      val _valueProperty: PropertyInfo =
          typeof(T).GetProperty("Value") ?? throw InvalidOperationException()

      func Cases() -> Dictionary<string, Type> {
          typeof(T)
              .GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic)
              .Where(t => _valueProperty.PropertyType.IsAssignableFrom(t))
              .ToDictionary(t => t.Name, t => t)
      }
  }
  ```

- **Expected:** Capturing `_valueProperty` in the lambda should produce a valid
  closure and the LINQ call chain should execute normally.
- **Observed:** The converter investigation produced invalid or nonsensical
  decompiled closure code for this pattern.
- **Likely area:** Lambda capture lowering for generic instance members,
  extension-method invocation lowering, and delegate construction for LINQ calls.
- **Suggested coverage:** Add a runtime codegen test with a generic class,
  captured instance field/property, and a LINQ `Where(...).ToDictionary(...)`
  chain.

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

### Declared `out var` invocation arguments lost their target type

- **Fixed:** Declared `out var` locals now use the target parameter type when the
  parameter exposes `RefKind.Out` with a plain element type.
- **Previous behavior:** `out var property` for metadata calls such as
  `JsonElement.TryGetProperty("value", out var property)` became an error-typed
  local. Overload resolution then failed quietly and codegen emitted
  `default(bool)` instead of a real call.
- **Coverage:** `ByRefParameterCodeGenTests` covers both source-defined `out`
  methods and metadata `out` methods.
