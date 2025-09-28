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

Compile with the LINQ reference to capture the current CLI diagnostics:

```
dotnet run --project src/Raven.Compiler -- src/Raven.Compiler/samples/linq.rav \
    --refs ~/.dotnet/packs/Microsoft.NETCore.App.Ref/9.0.9/ref/net9.0/System.Linq.dll
```

The invocation currently fails long before we reach code generation. The CLI
reports two diagnostics:

```
error RAV1501: No overload for method 'Where' takes 1 arguments
error RAV2200: Cannot infer the type of parameter 'value'. Specify an explicit
type or use the lambda in a delegate-typed context
```

The first error shows that we still do not recognize metadata `Where`
definitions as extension methods when a lambda argument is present, so overload
resolution thinks the call is missing the `source` argument altogether. The
second error is the familiar inference gap—the lambda never receives a target
delegate type while overload resolution is still considering both overloads, so
the binder immediately falls back to the error type and complains about the
unannotated parameter.【395e77†L1-L7】

Annotating the lambda parameters does not change the outcome. Replacing the
predicate with `func (value: int) -> bool` still produces `RAV1501`, confirming
that we never make it past extension recognition even when the delegate shape is
fully specified.【4b9c5d†L1-L4】

Fixing the first error requires tightening the extension lookup so that the
metadata-backed `Enumerable.Where` overloads flow through
`BindMemberAccessExpression` as true extension candidates. Only then will the
lambda binder see an extension receiver and have a chance to replay delegates.
Once that is in place we still need to address the second error by pushing
tentative delegate shapes into lambda binding so inference can disqualify the
index-aware overload the same way Roslyn does.

Once the binder recognizes the metadata extensions, add targeted semantic tests
that import the standard reference assemblies and the Raven LINQ fixture to
prove implicit lambda parameters bind without diagnostics. These tests should
guard both the metadata path (`System.Linq.Enumerable.Where`) and the
fixture-backed helpers.【F:test/Raven.CodeAnalysis.Tests/Semantics/ExtensionMethodSemanticTests.cs†L245-L308】【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L354-L472】

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

Once the binder recognizes the extension invocation, rerun the command with
`--bound-tree --no-emit` to capture the baseline bound structure for future
regression tests. The previous snapshot that recorded an explicit extension
receiver is now stale because the current pipeline fails before binding
completes.【e91466†L1-L24】
