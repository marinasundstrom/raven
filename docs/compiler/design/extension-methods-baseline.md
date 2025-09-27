# Extension Method Baseline Audit

Stage 1 of the extension-methods plan captures the current compiler behavior for
consuming extension methods compiled from other .NET languages and records the
repro that motivated the work.

## Pipeline overview

* **Import scope discovery.** `ImportBinder` exposes namespaces and type-scope
  imports, and its `LookupExtensionMethods` implementation forwards receiver
  types through `Binder.GetExtensionMethodsFromScope`, deduplicating candidates
  gathered from namespace and static type imports.【F:src/Raven.CodeAnalysis/Binder/ImportBinder.cs†L7-L148】
* **Method group formation.** When binding `a.Where(...)`, `BlockBinder` merges
  instance methods with the imported extension candidates and produces a
  `BoundMethodGroupExpression` that tracks both the original receiver and the
  extension receiver placeholder.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1946-L1984】
* **Invocation lowering.** `BoundInvocationExpression` records the method, the
  argument list, the syntactic receiver, and an optional extension receiver so
  that the lowerer can rewrite metadata extensions into static calls with the
  receiver inserted as the first argument.【F:src/Raven.CodeAnalysis/BoundTree/BoundInvocationExpression.cs†L5-L30】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.cs†L44-L66】
* **Code generation.** During emission, `ExpressionGenerator.EmitLambdaExpression`
  converts metadata delegate symbols to CLR types by calling
  `TypeSymbolExtensions.GetClrType` and then looks up the expected
  `(object, IntPtr)` constructor through the metadata load context. The existing
  `GetConstructor` call is the source of the MetadataLoadContext failure.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L360-L401】【F:src/Raven.CodeAnalysis/TypeSymbolExtensions.cs†L5-L122】

## Raven-authored gaps

* `SourceMethodSymbol.ComputeIsExtensionMethod` only returns `true` when a
  method is static, has parameters, and is decorated with `ExtensionAttribute`.
  There is no syntax to declare the receiver parameter or ensure the attribute
  is applied automatically, so source-authored extensions never enter the lookup
  pipeline yet.【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L197-L233】
* The CLI includes `System.Runtime` and `System.Collections` but leaves the
  `System.Linq` reference commented out. As a result, metadata-defined LINQ
  extensions are unavailable unless the user passes `--refs` manually, which
  masks the later emission bug.【F:src/Raven.Compiler/Program.cs†L153-L209】
* Even once Stage 3 starts flagging Raven-authored extensions, the binder needs
  to thread those methods through the same lookup path that feeds metadata
  helpers. `GetExtensionMethodsFromScope` today walks namespaces and static
  containers without validating the receiver, while `BlockBinder` merges the
  discovered candidates with instance members before handing the group to
  overload resolution. We'll need to tighten that pipeline so source-defined
  receivers respect accessibility, import scopes, and the synthetic extension
  receiver emitted in `BoundInvocationExpression`.【F:src/Raven.CodeAnalysis/Binder/Binder.cs†L187-L245】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1946-L2001】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3044-L3079】

## Minimal LINQ reproduction

```
import System.*
import System.Collections.Generic.*
import System.Linq.*

let numbers = [1, 2, 3]
let result = numbers.Where(func (value: int) -> bool => value == 2)
```

Compile with the LINQ reference to hit the MetadataLoadContext failure:

```
dotnet run --project src/Raven.Compiler -- src/Raven.Compiler/samples/linq.rav \
    --refs ~/.dotnet/packs/Microsoft.NETCore.App.Ref/9.0.9/ref/net9.0/System.Linq.dll
```

The compiler crashes while resolving the delegate constructor used for the
lambda passed to `Where` inside `ExpressionGenerator.EmitLambdaExpression`.【1907c1†L1-L24】

Removing the explicit parameter annotation exposes a second issue that blocks
parity with C#. `System.Linq.Enumerable.Where` ships two overloads: the
two-parameter predicate used above and an index-aware overload that expects a
`Func<TSource, int, bool>`. Raven's overload resolution only supplies a target
delegate type once a single candidate remains, so the lambda appears in an
untyped context while both overloads are viable. The binder therefore reports
`RAV2200` (“Cannot infer the type of parameter 'value'”) and the sample only
compiles if the lambda's parameters are annotated manually. Fixing this gap will
require pushing tentative delegate shapes into lambda binding so inference can
disqualify overloads the same way Roslyn does.

### Lambda inference fallout

Tracing the repro through the binder shows why we fail today. When the lambda
argument sits in an overloaded call, `BlockBinder.GetTargetType` tries to infer
the delegate type by examining every viable candidate method. Because
`FilterMethodsForLambda` only prunes candidates by parameter *count*, both LINQ
overloads survive this pass, and the helper gives up as soon as more than one
candidate remains.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2094-L2167】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3120-L3159】
Without a target delegate, `BindLambdaExpression` falls back to
`Compilation.ErrorTypeSymbol` for each unannotated parameter and reports
`RAV2200`, leaving overload resolution no information to disambiguate the call
later on.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1056-L1109】

To reach parity with Roslyn we need to cache the delegate shapes offered by
every overload while the lambda is still “unconverted.” One option is to teach
`GetTargetType` to return a small object that wraps the candidate delegate types
instead of a single `ITypeSymbol`, then have `BindLambdaExpression` produce a
`BoundLambdaExpression` that records the available signatures without reporting
diagnostics. Overload resolution can then apply the usual conversions, pushing
the lambda body through whichever delegate survives inference.

Run the same command with `--bound-tree --no-emit` to capture the baseline bound
structure for future regression tests. The invocation currently binds to a
metadata `Where` extension with a lambda parameter and records an explicit
extension receiver.【e91466†L1-L24】
