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
- Control-flow constructs such as `if`/`else`, loops, and user `try`/`catch` blocks now rewrite their conditions and bodies through the async lowerer so awaits nested inside branches or exception handlers expand into the same scheduling/resume pattern instead of leaving `BoundAwaitExpression` nodes behind.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L710-L829】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L99-L210】
- Expression-based `try` forms lower through the async rewriter as well, so awaits that appear inside `try` expressions schedule/resume correctly while preserving the surrounding exception flow.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L876-L910】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L385-L417】
- Hoisted locals that require disposal are tracked on the synthesized state machine, appended to the rewritten blocks so they drain at scope exit, and flushed through the `catch` path to preserve `using` semantics even when awaits suspend execution.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L204-L617】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L12-L106】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L368-L417】
- Async method bootstrap instantiates the synthesized struct, copies `this`/parameter values into fields, seeds `_state`, initializes `_builder`, and routes execution through `builder.Start(ref stateMachine)` (falling back to `MoveNext`) before returning the builder’s `Task`, matching the expected runtime handshake. File-scope code reuses the same bootstrap via the cached rewritten block for the implicit `Program.Main`.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L95-L170】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L41-L260】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L150-L226】
- The synthesized type implements `System.Runtime.CompilerServices.IAsyncStateMachine`, exposes a forwarding `SetStateMachine`, and the rewriter now drives both `SetException` on failures and `SetResult` on successful returns so the async builder observes the full completion contract.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L49-L260】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L191-L320】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L833-L874】
- Code generation now materializes synthesized async state machine structs, defines their hoisted fields and builder members, emits the stored `MoveNext`/`SetStateMachine` bodies, and decorates the original async method with `AsyncStateMachineAttribute` and `AsyncMethodBuilderAttribute` so the runtime observes the async metadata.【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L70-L141】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L61-L120】【F:src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs†L161-L239】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1-L121】
- Async method bootstraps now reuse the invocation receiver-address helper when loading `_builder`, so returning the builder task emits `ldflda` followed by a `call` to `AsyncTaskMethodBuilder.get_Task` instead of copying the struct into temporaries. The regression test suite asserts the getter call uses `call` and that no `stloc` occurs between the field address load and the invocation, preventing the invalid-program fault from resurfacing.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2056-L2080】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L120-L142】
- Constructed async helper invocations (e.g., builder generics) resolve through `ConstructedMethodSymbol.GetMethodInfo`, which translates every type argument into the compilation's `MetadataLoadContext` before asking reflection for the matching runtime method. This keeps emission within the metadata world and avoids falling back to ad-hoc `Assembly.Load` calls when the state machine type itself is synthesized.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L1-L132】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2640-L2745】
- Lambda expressions now accept an optional `async` modifier, allowing awaits inside both simple and parenthesized forms. The binder propagates `IsAsync` onto `SourceLambdaSymbol`, infers `Task<T>` or `Task` return types when no annotation is provided, and ensures lambda bodies convert to the awaited result type before defaulting to builder-friendly tasks.【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/ExpressionSyntaxParser.cs†L393-L521】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceLambdaSymbol.cs†L10-L75】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L12-L279】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLambdaTests.cs†L1-L82】
- Async functions and lambdas surface diagnostics when explicitly annotated with non-`Task` return types so authors receive targeted guidance to use `Task`/`Task<T>` while inference continues to supply the appropriate builder-friendly task shapes.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L295-L329】【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L36-L87】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L119-L187】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L1-L69】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLambdaTests.cs†L1-L120】

### Runtime readiness

- The `SpecialType` enumeration already contains the async method builder types, `Task`, and the attribute metadata we will need, indicating the compilation layer can resolve the required framework symbols once lowering consumes them.【F:src/Raven.CodeAnalysis/SpecialType.cs†L39-L58】
- The sample suite now includes an `async-await.rav` program that exercises the generated state machine and asserts the expected interleaving once the emitted IL executes without runtime faults.【F:src/Raven.Compiler/samples/async-await.rav†L1-L17】【F:test/Raven.CodeAnalysis.Samples.Tests/SampleProgramsTests.cs†L12-L118】
- `ilverify` no longer reports byref-of-byref faults when inspecting the sample assembly. The remaining verifier noise is limited to `System.Console` load failures because the framework facade is not passed to the tool, confirming the state machine now satisfies structured-exit requirements.【77ab35†L1-L7】
- Running the generated assembly prints the first awaited line (`first:1`) before stalling indefinitely instead of throwing `InvalidProgramException`, indicating the builder now survives the initial suspension but the continuation never drives the method to completion.【b3f15d†L1-L2】

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

### Step-by-step plan

#### Completed steps

1. **Surface synthesized `Program` members during setup.** `Compilation.Setup` now pre-creates the implicit `Program` shell and async entry points so tests that only call `EnsureSetup()` can discover the synthesized symbols.【F:src/Raven.CodeAnalysis/Compilation.cs†L129-L284】【F:src/Raven.CodeAnalysis/SemanticModel.cs†L573-L657】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L65-L109】
2. **Short-circuit async return diagnostics.** Async symbol binders flag invalid return annotations and the return binder honours the flag, preventing cascading conversion diagnostics while keeping builder synthesis intact. Regression tests cover both method and lambda scenarios.【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L49-L114】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L258-L347】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L455-L492】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L126-L180】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L38-L63】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLambdaTests.cs†L76-L114】
3. **Restore accessor diagnostics on error types.** Async property/indexer binders now keep the declared type syntax when resolution falls back to `ErrorTypeSymbol`, preserving `AsyncReturnTypeMustBeTaskLike` so authors still receive guidance when imports are missing. Regression coverage locks in the getter diagnostics for both properties and indexers.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L888-L1002】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L1046-L1210】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L139-L206】
4. **Audit value-type receiver handling.** State-machine setup now marks every builder mutation that operates on the synthesized struct with `RequiresReceiverAddress`, letting emission load addresses instead of copying value-type receivers. New IL-focused tests assert the generated code stores the builder and invokes `Start`/`SetResult` through field addresses so mutations land on the real state-machine instance.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L120-L214】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L928-L999】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L67-L151】
5. **Rebuild integration coverage.** Additional regression tests now check that multi-await methods hoist each awaiter, thrown exceptions flow through `SetException`, async accessors emit `SetResult`, and async lambdas synthesize their own state machines.【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L375-L508】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L153-L197】
6. **Return builder tasks by reference.** Property emission shares the invocation address-loading path so async bootstraps invoke `AsyncTaskMethodBuilder.get_Task` via `call` on `_builder`, and IL tests verify the getter consumes the field address without introducing temporary locals.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2056-L2080】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L120-L142】
7. **Add execution regression coverage.** The samples test suite now captures the async sample's standard streams, fails loudly when the runtime still throws `InvalidProgramException`, and will keep the fix honest once the remaining IL issues are resolved.【F:test/Raven.CodeAnalysis.Samples.Tests/SampleProgramsTests.cs†L19-L90】
8. **Write through the real state machine instance.** Field assignments emitted inside async `MoveNext` now reuse the invocation-address helpers to reload `self` via `ldarg.0`, letting `_state`, hoisted locals, and awaiters mutate the original struct without round-tripping through byref locals. The emitter understands address-of expressions that wrap `self`, so await scheduling can re-emit the receiver when it needs to evaluate the RHS first.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1644-L1724】

#### Upcoming steps

This roadmap keeps momentum on polishing the shipped async surface while sequencing runtime validation and documentation in tandem with the remaining binder/lowerer work.

- Investigate why `MainAsync` never prints `sum`/`done` despite the state machine resuming once, focusing on how the builder schedules continuations after the first await and whether the resumed path races the `_state` dispatch or hoisted locals.【b3f15d†L1-L2】

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
