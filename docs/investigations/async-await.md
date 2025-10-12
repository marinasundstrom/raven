# Async/await implementation investigation

## Current status

### Language surface area

- The language specification now spells out `await` semantics, covering context requirements, the awaited-pattern members, and result typing so the observable behaviour matches the implemented pipeline.【F:docs/lang/spec/language-specification.md†L337-L356】
- Grammar rules surface `async` for both top-level `func` statements and member declarations, and the statement parser now records the modifier before forwarding local functions to the binder. Method declarations retain the modifier through parsing as well.【F:docs/lang/spec/grammar.ebnf†L13-L42】【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/StatementSyntaxParser.cs†L391-L436】【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/TypeDeclarationParser.cs†L426-L503】
- Lambda syntax now accepts an optional `async` modifier so awaits inside simple and parenthesized lambdas project into async state machines while preserving the existing grammar surface.【F:docs/lang/spec/grammar.ebnf†L193-L200】【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/ExpressionSyntaxParser.cs†L393-L521】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceLambdaSymbol.cs†L10-L75】
- Property and indexer accessors accept an `async` modifier, allowing both block and expression-bodied getters/setters to participate in async lowering alongside method and local-function declarations.【F:docs/lang/spec/grammar.ebnf†L21-L43】【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/TypeDeclarationParser.cs†L604-L671】
- File-scope code now synthesizes a synchronous `Program.Main` entry point and, when awaits appear in global statements, an async `Program.MainAsync` that returns `System.Threading.Tasks.Task`/`Task<int>`. The binder routes top-level statements through `MainAsync` so they gain an async context, while the emitted `Main` bridge invokes `MainAsync(args).GetAwaiter().GetResult()` before returning the awaited result.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedMainMethodSymbol.cs†L5-L48】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedMainAsyncMethodSymbol.cs†L1-L52】【F:src/Raven.CodeAnalysis/SemanticModel.cs†L623-L688】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L89-L139】

### Symbol and binding pipeline

- Local `func` binders and type member binders respect the parsed modifier, set `SourceMethodSymbol.IsAsync`, and default unspecified return types to `System.Threading.Tasks.Task`. The method symbol now tracks whether its body contains awaits so later passes can trigger lowering.【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L50-L103】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L200-L353】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L28-L167】
- `await` expressions bind through a dedicated `BoundAwaitExpression` that validates the `GetAwaiter`/`IsCompleted`/`GetResult` pattern and enforces async-context requirements before capturing the awaiter/result types for downstream phases.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L555-L620】【F:src/Raven.CodeAnalysis/BoundTree/BoundAwaitExpression.cs†L7-L33】
- Accessor binders propagate `async` onto synthesized getter/setter methods, validate that async getters expose task-shaped property/indexer types, and allow async setters to lower through the runtime `AsyncVoidMethodBuilder`. Violations report `AsyncReturnTypeMustBeTaskLike` even when the declared type binds to `ErrorTypeSymbol`, keeping the guidance visible when imports are missing.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L888-L1002】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L1046-L1210】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L1-L120】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L139-L206】

### Lowering and code generation

- `CodeGenerator.EnsureAsyncStateMachines` now walks async methods, local functions, accessors, expression-bodied declarations, and file-scope statements, asks `AsyncLowerer` to analyze their bound bodies, and rewrites anything containing `await`, ensuring a synthesized state machine exists for every async entry point.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L620-L751】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L11-L172】
- The rewrite stage captures the original block, materializes a `SynthesizedAsyncStateMachineTypeSymbol`, and now emits a `MoveNext` that performs `_state` dispatch, hoists each awaiter into a synthesized field, and expands expression-statement awaits into scheduling blocks that set `_state`, call `AwaitUnsafeOnCompleted`/`AwaitOnCompleted`, and resume through labeled continuations that reset `_state = -1` before invoking `GetResult`. Completion now sets `_state = -2` and routes user `return` statements through the method builder’s `SetResult`, forwarding awaited values or expression results so the async contract observes the caller-provided value.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L12-L278】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L41-L107】
- Await lowering walks the async body up front to hoist locals that remain live across awaits into synthesized state-machine fields, rewriting declarations, assignments, and reads to target those fields so resumed execution observes the same values that were in scope before suspension. Await expressions now lower in return values, assignments, arguments, and other expression contexts by expanding to block expressions that stage the awaiter, schedule the continuation, and project the resumed result back into the surrounding expression tree.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L620-L1121】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L245-L417】
- Resume paths now capture the hoisted awaiter into a temporary local, clear the state-machine field before invoking `GetResult`, and forward evaluation through the cached awaiter so completed awaiters do not leak across suspensions.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1236-L1293】【F:src/Raven.CodeAnalysis/BoundTree/BoundDefaultValueExpression.cs†L1-L26】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L208-L223】
- Control-flow constructs such as `if`/`else`, loops, and user `try`/`catch` blocks now rewrite their conditions and bodies through the async lowerer so awaits nested inside branches or exception handlers expand into the same scheduling/resume pattern instead of leaving `BoundAwaitExpression` nodes behind.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L710-L829】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L99-L210】
- Expression-based `try` forms lower through the async rewriter as well, so awaits that appear inside `try` expressions schedule/resume correctly while preserving the surrounding exception flow.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L876-L910】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L385-L417】
- Hoisted locals that require disposal are tracked on the synthesized state machine, appended to the rewritten blocks so they drain at scope exit, and flushed through the `catch` path to preserve `using` semantics even when awaits suspend execution.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L204-L617】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L12-L106】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L368-L417】
- Async method bootstrap instantiates the synthesized struct, copies `this`/parameter values into fields, seeds `_state`, initializes `_builder`, and routes execution through `builder.Start(ref stateMachine)` (falling back to `MoveNext`) before returning the builder’s `Task`, matching the expected runtime handshake. File-scope code reuses the same bootstrap via the cached rewritten block for the implicit `Program.Main`.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L95-L170】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L41-L260】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L150-L226】
- The synthesized type implements `System.Runtime.CompilerServices.IAsyncStateMachine`, exposes a forwarding `SetStateMachine`, and the rewriter now drives both `SetException` on failures and `SetResult` on successful returns so the async builder observes the full completion contract.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L49-L260】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L191-L320】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L833-L874】
- Code generation now materializes synthesized async state machine structs, defines their hoisted fields and builder members, emits the stored `MoveNext`/`SetStateMachine` bodies, and decorates the original async method with `AsyncStateMachineAttribute` and `AsyncMethodBuilderAttribute` so the runtime observes the async metadata.【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L70-L141】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L61-L120】【F:src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs†L161-L239】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1-L121】
- Async method bootstraps now reuse the invocation receiver-address helper when loading `_builder`, so returning the builder task emits `ldflda` followed by a `call` to `AsyncTaskMethodBuilder.get_Task` instead of copying the struct into temporaries. The regression test suite asserts the getter call uses `call` and that no `stloc` occurs between the field address load and the invocation, preventing the invalid-program fault from resurfacing.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2056-L2080】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L120-L142】
- Return statements now detect when the compiler-synthesized `Task.CompletedTask` placeholder flows through a parameterless builder and treat it as an effectively-void return, ensuring the state machine still stamps `_state = -2` and calls `SetResult()` so runtimes observing the builder task see completion instead of hanging on the implicit fall-through. Regression IL tests pin the parameterless `SetResult()` call for both implicit fall-through and explicit `return Task.CompletedTask` paths.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L328-L354】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L147-L195】
- Task-returning helpers that produce values now surface the generic `AsyncTaskMethodBuilder<T>` through `AsyncMethodBuilderAttribute`, and their state-machine `MoveNext` bodies load the hoisted parameter before calling `SetResult(T)`, aligning with the documentation sample that returns the awaited argument.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L41-L102】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L205-L228】
- The emitter now rebinds async-builder invocations to the constructed state-machine builder type before loading metadata, so calls like `SetResult` and `SetException` target the closed generic overloads that match the synthesized field. The IL harness asserts that `MoveNext` invokes `AsyncTaskMethodBuilder<int>.SetResult(int)` to guard against regressions that would reopen the generic argument.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2507-L2607】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L239-L274】
- Constructed async helper invocations (e.g., builder generics) resolve through `ConstructedMethodSymbol.GetMethodInfo`, which translates every type argument into the compilation's `MetadataLoadContext` before asking reflection for the matching runtime method. This keeps emission within the metadata world and avoids falling back to ad-hoc `Assembly.Load` calls when the state machine type itself is synthesized.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L1-L132】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2640-L2745】
- Lambda expressions now accept an optional `async` modifier, allowing awaits inside both simple and parenthesized forms. The binder propagates `IsAsync` onto `SourceLambdaSymbol`, infers `Task<T>` or `Task` return types when no annotation is provided, and ensures lambda bodies convert to the awaited result type before defaulting to builder-friendly tasks.【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/ExpressionSyntaxParser.cs†L393-L521】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceLambdaSymbol.cs†L10-L75】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L12-L279】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLambdaTests.cs†L1-L82】
- Async functions and lambdas surface diagnostics when explicitly annotated with non-`Task` return types so authors receive targeted guidance to use `Task`/`Task<T>` while inference continues to supply the appropriate builder-friendly task shapes.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L295-L329】【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L36-L87】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L119-L187】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L1-L69】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLambdaTests.cs†L1-L120】

### Outstanding issues

- Raven currently emits invalid metadata for async methods that return values. Recompiling the sample program through `ravc` produces a `BadImageFormatException` at runtime, confirming the generated assembly cannot be loaded by the CLR.【c5d584†L1-L8】 A C# baseline exercising the same control flow compiles cleanly and uses a strongly-typed `AsyncTaskMethodBuilder<int>` field on its state machine, so `SetResult(int32)` receives the awaited value before the builder task is returned.【F:docs/investigations/snippets/async-return-value-csharp.cs†L1-L16】【F:docs/investigations/snippets/async-return-value-csharp.il†L1-L24】【F:docs/investigations/snippets/async-return-value-csharp.il†L26-L41】 In contrast the Raven emitter stamps the builder field as `AsyncTaskMethodBuilder<!0>` even though the synthesized state machine is not generic, and the subsequent `SetResult` call site remains open over `var`. The bad generic reference explains the runtime failure and highlights the gap to close when specializing builder fields during lowering.【F:docs/investigations/snippets/async-return-value-raven.il†L1-L32】【F:docs/investigations/snippets/async-return-value-raven.il†L34-L43】

### Remediation plan for Task-returning async methods

1. **(Complete) Propagate the awaited result type into state-machine synthesis.** `SynthesizedAsyncStateMachineTypeSymbol` now threads the `Task<T>` payload type into `DetermineBuilderType`, so async methods that return values request `AsyncTaskMethodBuilder<T>` rather than the open builder placeholder. This specialization flows through local functions, accessors, and top-level async entry points.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L187-L208】
2. **(Complete) Specialize builder member lookups at emission time.** `ExpressionGenerator` resolves async-builder invocations against the constructed builder symbol so the runtime sees closed overloads for `Start`, `SetException`, and `SetResult`. The IL regression test pins the `AsyncTaskMethodBuilder<int>.SetResult(int)` call shape.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2507-L2607】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L239-L274】
3. **(Active) Emit `_builder` with a closed runtime type.** Even with the updated symbol, the generated IL still stamps the field as `AsyncTaskMethodBuilder<!0>`, so the runtime fails the `Task<int>` state machine. Next steps:
   - Inspect the `FieldInfo.FieldType` emitted by `TypeGenerator` for value-returning async methods to confirm whether `DefineField` still receives an open generic runtime type.
   - Trace `ResolveClrType`/`GetClrType` for `AsyncTaskMethodBuilder<T>` and ensure the metadata load context returns a constructed `Type` instead of the generic definition.【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L296-L314】
   - Adjust the emission path so `TypeBuilder.DefineField` always sees the closed builder type (and add a regression test that reads the field signature from metadata once the fix lands).
4. **Validate end-to-end runtime behaviour.** Once the `_builder` field closes over the awaited type, re-run the failing repro (`dotnet run -- samples/test7.rav -o test.dll -d pretty && dotnet test.dll`), capture the disassembly, and extend the investigation with the successful execution trace.【c5d584†L1-L8】

### Runtime readiness

- The `SpecialType` enumeration already contains the async method builder types, `Task`, and the attribute metadata we will need, indicating the compilation layer can resolve the required framework symbols once lowering consumes them.【F:src/Raven.CodeAnalysis/SpecialType.cs†L39-L58】
- The sample suite now includes an `async-await.rav` program that exercises the generated state machine and asserts the expected interleaving once the emitted IL executes without runtime faults.【F:src/Raven.Compiler/samples/async-await.rav†L1-L17】【F:test/Raven.CodeAnalysis.Samples.Tests/SampleProgramsTests.cs†L12-L118】
- Authors can experiment with the async surface area using the following program, which imports the console and task namespaces, awaits a helper that resumes with the provided value, and writes the awaited result from top-level code:

  ```rav
  import System.Console.*
  import System.Threading.Tasks.*

  async func Test(value: int) -> Task<int> {
      await Task.Delay(10)
      return value
  }

  let x = await Test(42)

  WriteLine(x)
  ```
- To benchmark the Raven lowering against the C# baseline, we mirrored the snippet as a C# console program and disassembled the
  resulting assembly with `ilspycmd --ilcode` after a release build. Keeping the C# source in the investigation makes it easy to
  regenerate IL when compiler changes warrant a fresh comparison:

  ```csharp
  using System;
  using System.Threading.Tasks;

  internal static class Program
  {
      private static async Task<int> Test(int value)
      {
          await Task.Delay(10);
          return value;
      }

      private static async Task Main()
      {
          var x = await Test(42);
          Console.WriteLine(x);
      }
  }
  ```
- The C# compiler produces two nested state-machine structs—`<Test>d__0` for the helper and `<Main>d__1` for the entry point.
  Each struct carries `<>1__state`, the appropriate `AsyncTaskMethodBuilder` instance, the awaited payload (`value`) or hoisted
  awaiter, and forwards `SetStateMachine` straight to the builder, matching the invariants Raven relies on.【F:docs/investigations/async-await.md†L143-L208】
- The async bootstraps materialize the structs on the stack, stamp `_state = -1`, and call `builder.Start(ref stateMachine)`
  before returning `builder.Task`, confirming the control flow we expect Raven to mirror during emission.【F:docs/investigations/async-await.md†L209-L248】
- `<Main>d__1.MoveNext` stores the awaited `Task<int>`'s awaiter into `<>u__1`, stamps `_state = 0`, schedules the continuation
  with `AwaitUnsafeOnCompleted`, and on resume clears the hoist, resets `_state = -1`, and feeds the awaited value into
  `Console.WriteLine`. The direct `stfld` writes show why Raven must avoid copying the state machine before mutating fields.【F:docs/investigations/async-await.md†L107-L175】
- `<Test>d__0.MoveNext` follows the same pattern: it caches the `TaskAwaiter` from `Task.Delay(10)`, updates `_state`, calls the
  generic builder’s `AwaitUnsafeOnCompleted`, and after the await retrieves the preserved `value` field so it can return through
  `AsyncTaskMethodBuilder<int>.SetResult`.【F:docs/investigations/async-await.md†L176-L248】
  - Rebuilding and running `samples/async-await.rav` via `dotnet run --no-build --project src/Raven.Compiler/Raven.Compiler.csproj -- src/Raven.Compiler/samples/async-await.rav -o /tmp/raven-samples/async-await/async-await.dll` now prints `first:1`, `sum:6`, and `done`, confirming the CompletedTask fallthrough fix unblocks runtime execution.【5edb92†L1-L9】
- `ilverify` no longer reports byref-of-byref faults when inspecting the sample assembly. The remaining verifier noise is limited to `System.Console` load failures because the framework facade is not passed to the tool, confirming the state machine now satisfies structured-exit requirements.【77ab35†L1-L7】
- Running the generated assembly now prints the awaited lines (`first:1`, `sum:6`, `done`) and exits normally, confirming the state machine advances through every continuation.【5edb92†L1-L9】
- A reflection harness that loads the emitted `Program.MainAsync` now reports `TaskStatus.RanToCompletion` and prints the awaited output (`first:1`, `sum:6`, `done`), confirming the builder drives the state machine through every continuation. The C# snippet below remains available for future diagnostics if the status regresses.【e2da1b†L1-L4】

  ```csharp
  var asm = Assembly.LoadFile("/tmp/async-await.dll");
  var mainAsync = asm.GetType("Program")!.GetMethod("MainAsync", BindingFlags.NonPublic | BindingFlags.Static)!;
  var task = (Task)mainAsync.Invoke(null, new object?[] { Array.Empty<string>() })!;
    Console.WriteLine(task.Status); // RanToCompletion
  Console.WriteLine(task.GetType().GetField("m_stateObject", BindingFlags.NonPublic | BindingFlags.Instance)!.GetValue(task));
  ```

   Keep this harness on hand in case a future regression reintroduces the hang—the field probes make it easy to inspect builder internals and confirm whether the state machine is still boxed correctly.

### IL comparison with the C# baseline

- Disassembling the Raven generated assembly and the mirrored C# sample shows that our `MoveNext` body repeatedly copies the state machine into temporaries before mutating fields, e.g. `_state`, `<>awaiter0`, and the hoisted locals. Each `stloc`/`ldloca` pair operates on a detached copy, so the actual state machine never observes the updated `_state` or awaiter assignments.【F:docs/investigations/async-await.md†L41-L57】
- The C# compiler instead writes through the by-ref `this` parameter using `ldarg.0` + `stfld`, ensuring `_state` and hoisted awaiters mutate in place before calling `AwaitUnsafeOnCompleted`. Our emitter must follow suit to avoid verifier failures when the runtime re-enters the method builder.【F:docs/investigations/async-await.md†L59-L68】

```il
// Raven async-await.rav MoveNext excerpt
IL_0099: ldarg.0
IL_009a: stloc.s 4
IL_009c: ldloca.s 4
IL_009e: ldc.i4.0
IL_009f: stfld int32 Program/'Program+<>c__AsyncStateMachine1'::_state
IL_00a4: ldarga.s 0
IL_00a6: ldflda valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder Program/'Program+<>c__AsyncStateMachine1'::_builder
IL_00ab: ldarg.0
IL_00ac: ldflda valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter Program/'Program+<>c__AsyncStateMachine1'::'<>awaiter0'
```

```il
// C# async-await sample MoveNext excerpt
IL_0034: ldarg.0
IL_0035: ldc.i4.0
IL_0036: dup
IL_0037: stloc.0
IL_0038: stfld int32 Program/'<Main>d__1'::'<>1__state'
IL_003d: ldarg.0
IL_003e: ldloc.s 4
IL_0040: stfld valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter Program/'<Main>d__1'::'<>u__1'
```

#### Required emitter changes

- `BoundFieldAssignmentExpression` lowering already marks async-state-machine writes as `requiresReceiverAddress: true`, but the emission path still spills value-type receivers before taking their address. When the receiver is `null`/`self`, `EmitAssignmentExpression` calls into `CanReEmitInvocationReceiverAddress` and `TryEmitInvocationReceiverAddress`, yet the `EmitValueTypeAddressIfNeeded` helper invoked afterwards persists in storing `ldarg.0` into a temporary and then loading its address, recreating the problematic `stloc`/`ldloca` sequence.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1180-L1307】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1641-L1709】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2338-L2406】
- Roslyn sidesteps the copy by emitting `ldarg.0`, immediately using `dup` when it needs to preserve the receiver, and calling `stfld`/`ldflda` directly on the in-place struct. To match that behaviour we need a Raven-side helper that recognises `this`/`BoundSelfExpression`/captured-struct locals and produces the address without spilling. One option is to add an `EmitValueTypeReceiverAddress` that emits `ldarg.0`, `ldflda`, or `ldelema` as appropriate and returns whether the receiver is already on the evaluation stack so callers can decide when to duplicate.【F:docs/investigations/async-await.md†L59-L68】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2284-L2360】
- Once the receiver stays by-ref we can re-order the store so the awaiter assignment mirrors the Roslyn pattern (`ldarg.0`, `ldc.i4`, `dup`, `stfld ...::_state`, `ldarg.0`, `stfld ...::<>awaiter0`). Updating the IL regression tests to assert there are no intermediary `stloc` opcodes between `ldarg.0` and the `stfld` that updates `_state` (and the hoisted awaiter fields) will keep the fix from regressing.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L120-L195】
- `async Task<T>` entry points still trip the runtime because the generated `MoveNext` mutates a copy of the state machine before calling into the generic builder, so `_state` never advances and the verifier throws `BadImageFormatException` when `AsyncMethodBuilderCore.Start` re-enters the struct. The repro in `src/Raven.Compiler/samples/test7.rav` mirrors the failing scenario.【F:src/Raven.Compiler/samples/test7.rav†L1-L11】 To align with Roslyn we need to extend the plan above with concrete emitter work:
  - Teach the assignment path to leave the actual struct on the evaluation stack when `requiresReceiverAddress: true`. Instead of the current `EmitValueTypeAddressIfNeeded` spill-and-load sequence we should synthesise a dedicated `EmitValueTypeReceiverAddress` that recognises state-machine `this`, captured-struct locals, and array elements, loading their address in place (via `ldarg.0`, `ldflda`, `ldelema`, etc.) without introducing temporaries.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2458-L2477】
  - Once the receiver stays by-ref, rewrite the await scheduling block so `_state`, awaiter hoists, and builder calls all target that by-ref instance. That matches the Roslyn order (`ldarg.0`, `ldc.i4`, `dup`, `stfld ...::_state`, `ldarg.0`, `ldfld`/`stfld`) and keeps `AwaitUnsafeOnCompleted` working when the runtime resumes the struct.
  - Ensure the builder dispatch uses the constructed generic when a concrete `Task<T>` flows out of the async method. `SynthesizedAsyncStateMachineTypeSymbol.DetermineBuilderType` already constructs `AsyncTaskMethodBuilder<T>` for non-error T, so the missing piece is emission: every place that currently spills the builder receiver before calling `SetResult`/`SetException`/`AwaitUnsafeOnCompleted` must use the same by-ref helper so we pass the builder field by address without copying the parent struct.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L190-L209】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1641-L1709】
  - Expand the IL regression suite with a `Task<int>` sample that asserts `_state`/awaiter stores are emitted without intermediate `stloc` and that the generated metadata references `AsyncTaskMethodBuilder<int>`. A high-level execution test that compiles and runs `test7.rav` will confirm the runtime no longer surfaces `BadImageFormatException`.

##### Strategy for unblocking `async Task<T>` entry points

1. **Codify the failing shape.** Extract the minimal repro (`samples/test7.rav`) into a compiler test that asserts the produced IL currently triggers `BadImageFormatException`, and add a Roslyn baseline disassembly so we always have the target control flow handy during the fix. This keeps the regression visible while we iterate on emitter updates.【F:src/Raven.Compiler/samples/test7.rav†L1-L11】
   - `AsyncILGenerationTests.AsyncEntryPoint_WithTaskOfInt_ThrowsBadImageFormatException` now emits the console-app repro, loads the generated `Program.Main`, and asserts the runtime surfaces `BadImageFormatException`, pinning the invalid state-machine shape until the emitter rewrite lands.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L47-L122】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L226-L273】
2. **Stabilise value-type receiver handling.** Implement the dedicated `EmitValueTypeReceiverAddress` helper and retrofit the async emission hot paths (`EmitAssignmentExpression`, builder calls, await hoists) to use it whenever `requiresReceiverAddress` is set. Gate the change with IL tests that scan for `stloc` between `ldarg.0` and subsequent `stfld`/`ldflda` opcodes inside async `MoveNext` bodies so future regressions are caught immediately.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1641-L1709】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2458-L2477】
3. **Rework await scheduling order.** Once the emitter writes straight through the real state machine, update the lowering templates so `_state` and awaiter assignments follow the Roslyn ordering, duplicating the receiver only when needed. Extend the IL harness to compare the opcode sequence around each await with the new canonical form (`ldarg.0`, `ldc.i4`, `dup`, `stfld`, `ldarg.0`, `stfld`).【F:docs/investigations/async-await.md†L59-L68】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L120-L195】
4. **Guarantee generic builder dispatch.** Audit every `AsyncTaskMethodBuilder<T>` interaction—`Start`, `SetResult`, `SetException`, `AwaitUnsafeOnCompleted`, and `AwaitOnCompleted`—to ensure we pass the builder field by ref and load metadata for the constructed generic. Add inspection tests that confirm `Program.MainAsync` references `AsyncTaskMethodBuilder<int>` and that the emitted signatures match the closed generic overloads.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L190-L209】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2338-L2406】
5. **Validate at runtime.** Promote the current manual repro into an automated integration test that compiles and executes the assembly, asserting `Program.MainAsync` returns a completed `Task<int>` and prints the awaited value. Run `ilverify` (or `peverify` on Windows) in CI to guarantee the state machine passes verifier checks once the by-ref fixes land.【F:src/Raven.Compiler/samples/test7.rav†L1-L11】
6. **Document the invariant.** Update the specification/investigation once the fix ships to call out that async entry points must never copy their state machine before mutating `_state` or invoking builder methods, and link the new regression tests so future contributors understand the requirement.【F:docs/lang/spec/language-specification.md†L337-L356】

### Reference: C# state machine layout

- Compiling a release-mode C# sample with both `Task`- and `Task<int>`-returning async methods produces
  one struct per method: `<ExampleAsync>d__1`, `<ExampleValueAsync>d__2`, and `<Main>d__0`. Each struct
  implements `IAsyncStateMachine`, hoists a `<>1__state` field, carries an `AsyncTaskMethodBuilder`
  (or its generic counterpart), and caches every active awaiter in a dedicated `<>u__*` field. The
  `SetStateMachine` implementation simply forwards to the builder, so the runtime handshake always
  flows through the builder APIs.【F:docs/investigations/async-await.md†L87-L121】
- The async method bootstrap allocates the struct, seeds `_state = -1`, initializes the builder, and
  calls `builder.Start(ref stateMachine)` before returning `builder.Task`. This is the same pattern
  Raven already emits and underlines why the struct must remain a value type whose builder field is
  always accessed by address.【F:docs/investigations/async-await.md†L93-L121】

```il
// Release Program.Main bootstrap
IL_0000: newobj instance void Program/'<Main>d__0'::.ctor()
IL_0005: stloc.0
IL_0006: ldloc.0
IL_0007: call valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder
                     [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder::Create()
IL_000c: stfld valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder
                     Program/'<Main>d__0'::'<>t__builder'
IL_0011: ldloc.0
IL_0012: ldc.i4.m1
IL_0013: stfld int32 Program/'<Main>d__0'::'<>1__state'
IL_0018: ldloc.0
IL_0019: ldflda valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder
                     Program/'<Main>d__0'::'<>t__builder'
IL_001e: ldloca.s 0
IL_0020: call instance void [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder::Start
                     <valuetype Program/'<Main>d__0'>(!!0&)
IL_0025: ldloc.0
IL_0026: ldflda valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder
                     Program/'<Main>d__0'::'<>t__builder'
IL_002b: call instance class [System.Runtime]System.Threading.Tasks.Task
                     [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder::get_Task()
```
- Within `MoveNext`, each `await` writes its awaiter into the hoisted field, stamps the continuation
  state, and schedules `AwaitUnsafeOnCompleted`. Resume paths reload the awaiter, clear the hoisted
  slot, reset `_state = -1`, and then call `GetResult` before continuing. Success funnels through the
  builder’s `SetResult`, while exceptions jump to the `catch` block that sets `_state = -2` and calls
  `SetException`. The `Task<int>` variant mirrors the same structure, but routes the final value into
  `AsyncTaskMethodBuilder<int>.SetResult(result)`.【F:docs/investigations/async-await.md†L122-L145】

```il
// Await scheduling in Program.<Main>d__0.MoveNext
IL_0025: ldarg.0
IL_0026: ldc.i4.0
IL_0027: dup
IL_0028: stloc.0
IL_0029: stfld int32 Program/'<Main>d__0'::'<>1__state'
IL_002e: ldarg.0
IL_002f: ldloc.2
IL_0030: stfld valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter Program/'<Main>d__0'::'<>u__1'
IL_0035: ldarg.0
IL_0036: ldflda valuetype [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder Program/'<Main>d__0'::'<>t__builder'
IL_003b: ldloca.s 2
IL_003d: ldarg.0
IL_003e: call instance void [System.Runtime]System.Runtime.CompilerServices.AsyncTaskMethodBuilder::AwaitUnsafeOnCompleted
                     <valuetype [System.Runtime]System.Runtime.CompilerServices.TaskAwaiter, valuetype Program/'<Main>d__0'>(!!0&, !!1&)
```
- The synthesized entry point wraps the awaited result in an interpolated string to demonstrate value
  flow: the state machine builds a `DefaultInterpolatedStringHandler`, stores the awaited integer into
  a local, and appends it before writing to `Console`. The `Task`-returning helper follows the same
  awaiter scheduling shape without a result slot, confirming that both builder types produce identical
  control-flow scaffolding aside from result storage.【F:docs/investigations/async-await.md†L146-L150】

## Requirements to match C# `Task`/`Task<T>` async behaviour

Implementing async methods and lambdas with C#-equivalent semantics relies on coordinated work across the parser, binder, lowering pipeline, and emitter:

1. **Surface the `async` modifier everywhere authors can write it.** The statement parser recognises `async func` declarations so top-level local functions can opt into async lowering, type/member parsers thread the modifier through method and accessor declarations, and the expression parser accepts `async` on both simple and parenthesised lambdas.【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/StatementSyntaxParser.cs†L28-L33】【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/TypeDeclarationParser.cs†L635-L668】【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/ExpressionSyntaxParser.cs†L390-L425】
2. **Bind async entry points with `Task`-shaped return types.** Function and member binders flip `IsAsync`, default missing annotations to `System.Threading.Tasks.Task`, and fall back to diagnostics plus a task return when authors pick anything else. Lambdas follow the same rule: they reject non-task annotations, infer `Task`/`Task<T>` from the body, and convert the body into the awaited result type. All of these checks flow through `Binder.IsValidAsyncReturnType`, which only admits `Task` and `Task<T>` while allowing error types to keep diagnostics visible.【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L51-L117】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L219-L452】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L29-L305】【F:src/Raven.CodeAnalysis/Binder/Binder.cs†L1416-L1434】
3. **Synthesize a matching async state machine and builder.** When lowering decides a method or lambda is async it materialises a `SynthesizedAsyncStateMachineTypeSymbol`, copies `this`/parameters into hoisted fields, seeds the state slot, and chooses the correct runtime builder—`AsyncTaskMethodBuilder` for `Task`, its generic counterpart for `Task<T>`, or the void builder for async accessors. The rewritten body ultimately returns the builder’s `Task` property so callers observe the expected `Task` or `Task<T>`.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L112-L190】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1658-L1675】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L176-L237】
4. **Emit runtime metadata and bridge synchronous entry points.** Code generation decorates async methods with `AsyncStateMachineAttribute`/`AsyncMethodBuilderAttribute` so the CLR recognises the generated struct, and the synthesized synchronous `Main` bridge invokes `MainAsync`, awaits its result, and forwards the return value, matching the Task-based entry-point pattern in C#.【F:src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs†L223-L263】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L116-L188】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L333-L368】

## Remaining work

### Resolved gaps

- **Synthesized `Program`/`MainAsync` exposure.** `Compilation.EnsureSetup()` now seeds
  the implicit `Program` container and its entry points so semantic queries after
  setup can see the async surface without forcing binder creation.【F:src/Raven.CodeAnalysis/Compilation.cs†L129-L284】【F:src/Raven.CodeAnalysis/SemanticModel.cs†L573-L657】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L65-L109】
- **Duplicate diagnostics for invalid async returns.** Async methods, functions, and
  lambdas mark their symbols when a non-`Task` annotation is rejected, allowing the
  return binder to skip conversions that previously emitted cascading `RAV1503`
  errors. Regression tests now assert only the primary async diagnostic surfaces for
  both expression-bodied and block-bodied returns.【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L49-L114】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L258-L347】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L455-L492】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L126-L180】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L25-L205】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceLambdaSymbol.cs†L6-L98】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L38-L63】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLambdaTests.cs†L76-L114】
- **Generic `Task<T>` parity audit.** The language specification now mirrors C# by
  stating that async bodies convert returned expressions to the awaited result type
  before calling `SetResult(value)`, covering both `Task` and `Task<T>` members.【F:docs/lang/spec/language-specification.md†L1296-L1326】
  Binder checks line up with that rule: when binding `return` statements inside async
  methods or lambdas, `BlockBinder.BindReturnStatement` unwraps the generic builder
  target, converts the expression to `T`, and reports diagnostics when a bare
  `return;` would leave a `Task<T>` without a value.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L454-L515】
  `MethodBodyBinder` follows up by skipping trailing-expression checks only for
  `async Task` and `async Task<Unit>` bodies so expression-bodied wrappers cannot leak
  non-unit results through implicit task completion.【F:src/Raven.CodeAnalysis/Binder/MethodBodyBinder.cs†L40-L79】
  On the lowering side, `AsyncLowerer` threads any converted expression into
  `AsyncTaskMethodBuilder<T>.SetResult(result)` and falls back to default values when
  the builder expects a parameter but the bound return had no result, matching C#'s
  contract for both explicit and implicit completions.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L268-L347】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L861-L907】
  Remaining investigation work focuses on filling coverage gaps (e.g., conversions
  from tuple expressions and nullable annotations) so future regressions cannot break
  the parity we now rely on.

### Step-by-step plan

#### Completed steps

1. **Surface synthesized `Program` members during setup.** `Compilation.Setup` now pre-creates the implicit `Program` shell and async entry points so tests that only call `EnsureSetup()` can discover the synthesized symbols.【F:src/Raven.CodeAnalysis/Compilation.cs†L129-L284】【F:src/Raven.CodeAnalysis/SemanticModel.cs†L573-L657】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L65-L109】
2. **Short-circuit async return diagnostics.** Async symbol binders flag invalid return annotations and the return binder honours the flag, preventing cascading conversion diagnostics while keeping builder synthesis intact. Regression tests cover both method and lambda scenarios.【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L49-L114】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L258-L347】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L455-L492】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L126-L180】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L38-L63】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLambdaTests.cs†L76-L114】
3. **Restore accessor diagnostics on error types.** Async property/indexer binders now keep the declared type syntax when resolution falls back to `ErrorTypeSymbol`, preserving `AsyncReturnTypeMustBeTaskLike` so authors still receive guidance when imports are missing. Regression coverage locks in the getter diagnostics for both properties and indexers.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L888-L1002】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L1046-L1210】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L139-L206】
4. **Audit value-type receiver handling.** State-machine setup now marks every builder mutation that operates on the synthesized struct with `RequiresReceiverAddress`, letting emission load addresses instead of copying value-type receivers. New IL-focused tests assert the generated code stores the builder and invokes `Start`/`SetResult` through field addresses so mutations land on the real state-machine instance.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L120-L214】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L928-L999】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L67-L151】
5. **Rebuild integration coverage.** Additional regression tests now check that multi-await methods hoist each awaiter, thrown exceptions flow through `SetException`, async accessors emit `SetResult`, and async lambdas synthesize their own state machines.【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L375-L508】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L153-L197】
6. **Return builder tasks by reference.** Property emission shares the invocation address-loading path so async bootstraps invoke `AsyncTaskMethodBuilder.get_Task` via `call` on `_builder`, and IL tests verify the getter consumes the field address without introducing temporary locals.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2056-L2080】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L120-L142】
7. **Add execution regression coverage.** The samples test suite now captures the async sample's standard streams, fails loudly when the runtime still throws `InvalidProgramException`, and will keep the fix honest once the remaining IL issues are resolved.【F:test/Raven.CodeAnalysis.Samples.Tests/SampleProgramsTests.cs†L19-L90】
8. **Write through the real state machine instance.** Field assignments emitted inside async `MoveNext` now reuse the invocation-address helpers to reload `self` via `ldarg.0`, letting `_state`, hoisted locals, and awaiters mutate the original struct without round-tripping through byref locals. The emitter understands address-of expressions that wrap `self`, so await scheduling can re-emit the receiver when it needs to evaluate the RHS first.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1655-L1788】
9. **Complete implicit `Task` returns.** Async methods that fall through without an explicit `return` previously left the builder task unfinished because the binder injected `Task.CompletedTask`, causing the rewriter to skip `SetResult()`. The lowering pass now recognizes the placeholder and still calls the parameterless builder, allowing runtime execution of the sample program to print `sum`/`done` without hanging.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L328-L354】【db673f†L1-L4】
10. **Lock in `Task.CompletedTask` coverage.** IL-focused regression tests assert that async state machines call the parameterless `SetResult()` when either falling through or explicitly returning `Task.CompletedTask`, ensuring future lowering tweaks keep the builder task completion contract intact.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L147-L195】
11. **Cover expression-bodied `Task.CompletedTask` returns.** Semantic regression tests pin the bound trees produced for expression-bodied async methods and getters that return `Task.CompletedTask`, ensuring the implicit return blocks continue to expose the property access that the async lowerer recognizes for parameterless completion.【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L229-L274】
12. **Assert state-machine field stores keep the receiver on the stack.** IL regression tests now walk every `_state`/`<>awaiter*` store emitted in `MoveNext` and fail if a `stloc` instruction appears between the `ldarg.0` receiver load and the `stfld`, keeping future emitter changes from reintroducing value-type spills.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L97-L150】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L226-L262】
13. **Clear hoisted awaiters before resuming execution.** The async lowerer now copies each hoisted awaiter into a temporary on resume, zeroes the state-machine field, and then evaluates `GetResult` through the cached awaiter. Regression IL coverage asserts every awaiter field receives a second store after scheduling to keep resumptions from retaining completed awaiters.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1236-L1293】【F:src/Raven.CodeAnalysis/BoundTree/BoundDefaultValueExpression.cs†L1-L26】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L208-L223】

#### Upcoming steps

1. **Close tuple conversion coverage.** Add semantic and lowering regression tests that return tuple expressions from `async Task<T>` members, ensuring `BlockBinder.BindReturnStatement` and `AsyncLowerer` preserve tuple element typing when forwarding the awaited result.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L454-L515】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L268-L354】
2. **Exercise nullable annotations.** Extend the async method/lambda suites with scenarios such as `async Task<int?>` returning nullable literals, conversions, and `default`, pinning both the binder diagnostics and emitted `SetResult` invocations so nullable metadata flows through unchanged.【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L38-L274】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L147-L223】
3. **Spec sync for conversion rules.** Update the language specification’s async-return section to explicitly call out tuple and nullable conversions so the documented behaviour matches the binder/lowerer coverage being added.【F:docs/lang/spec/language-specification.md†L1296-L1326】
4. **Audit async lambda inference.** Add targeted tests for `async` lambdas returning `Task<T>` in delegate and expression-tree contexts to make sure `SourceLambdaSymbol` inference keeps the awaited result type intact when conversions apply.【F:src/Raven.CodeAnalysis/Symbols/Source/SourceLambdaSymbol.cs†L10-L75】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L12-L279】

This roadmap keeps momentum on polishing the shipped async surface while sequencing runtime validation and documentation in tandem with the remaining binder/lowerer work.

> **Testing note:** When expanding the regression suite, prefer the generic notation `Task<int>` (with angle brackets) rather than indexer-style spellings such as `Task[int]` so expectations match the emitted metadata and existing tests.【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLambdaTests.cs†L23-L27】

## Async enumerable roadmap

### Proposed syntax

```raven
async func Iterate() -> IAsyncEnumerable<int> {
    let x = await DoSomething()
    yield return x
}

async func Consume() -> Task<int> {
    var total = 0
    await for x in Iterate() {
        total += x
    }

    return total
}
```

This shape keeps async enumerables aligned with existing iterator declarations: an `async` function that returns `IAsyncEnumerable<T>` and contains `yield` statements becomes an async iterator. Both `await` and standard `yield` operations remain valid inside the body so authors can interleave asynchronous work with incremental element production. Consumers iterate with `await for`, allowing suspension while awaiting the next asynchronous element.

### Step-by-step plan

1. **Design the async iteration surface.** Decide on syntax (e.g., `async iter func`) and specification changes that allow authors to declare async iterators returning `IAsyncEnumerable<T>` while keeping the grammar compatible with existing iterator forms.【F:docs/lang/spec/grammar.ebnf†L13-L200】【F:docs/lang/spec/language-specification.md†L329-L344】
2. **Bind async enumerators.** Teach method and function binders to recognize async-iterator modifiers, validate that declarations return `IAsyncEnumerable<T>`/`IAsyncEnumerator<T>`, and surface targeted diagnostics when the signature is invalid.【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L36-L103】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L200-L353】
3. **Lower async iterator bodies.** Extend `AsyncLowerer` (or a companion pass) to produce enumerator state machines that integrate builder scheduling with `MoveNextAsync`, hoist locals, and preserve disposal semantics across suspensions.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L12-L1121】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L12-L260】
4. **Emit async enumerable metadata.** Update the emitter to synthesize iterator structs/classes, stamp methods with the appropriate async iterator attributes, and ensure generated IL matches the `IAsyncEnumerable<T>` contract.【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L70-L141】【F:src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs†L161-L239】
5. **Add execution coverage and documentation.** Introduce integration tests that execute async enumerators, validate the yielded values and disposal behaviour, and document the new feature in the language specification alongside guidance for library authors.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1-L121】【F:docs/lang/spec/language-specification.md†L329-L344】

## Future features

- **Async streams enhancements.** After landing the core async enumerable pipeline, explore advanced features such as `await foreach`, cancellation hooks, and configurability to align with modern .NET async stream patterns.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L12-L278】【F:docs/lang/spec/language-specification.md†L329-L344】
