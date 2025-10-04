# Async/await implementation investigation

## Current status

### Language surface area

- The language specification still reserves `await` as a unary expression without defining the asynchronous behaviour, so the parser can recognize the token while the observable semantics remain open.【F:docs/lang/spec/language-specification.md†L329-L336】
- Grammar rules surface `async` for both top-level `func` statements and member declarations, and the statement parser now records the modifier before forwarding local functions to the binder. Method declarations retain the modifier through parsing as well.【F:docs/lang/spec/grammar.ebnf†L13-L42】【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/StatementSyntaxParser.cs†L391-L436】【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/TypeDeclarationParser.cs†L426-L503】
- Lambda syntax now accepts an optional `async` modifier so awaits inside simple and parenthesized lambdas project into async state machines while preserving the existing grammar surface.【F:docs/lang/spec/grammar.ebnf†L193-L200】【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/ExpressionSyntaxParser.cs†L393-L521】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceLambdaSymbol.cs†L10-L75】
- Property and indexer accessors accept an `async` modifier, allowing both block and expression-bodied getters/setters to participate in async lowering alongside method and local-function declarations.【F:docs/lang/spec/grammar.ebnf†L21-L43】【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/TypeDeclarationParser.cs†L604-L671】
- File-scope code now synthesizes `Program.Main` as an async entry point that returns `System.Threading.Tasks.Task`, so top-level statements inherit an async context and may use `await` without additional scaffolding.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedMainMethodSymbol.cs†L5-L40】【F:src/Raven.CodeAnalysis/Compilation.cs†L198-L236】

### Symbol and binding pipeline

- Local `func` binders and type member binders respect the parsed modifier, set `SourceMethodSymbol.IsAsync`, and default unspecified return types to `System.Threading.Tasks.Task`. The method symbol now tracks whether its body contains awaits so later passes can trigger lowering.【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L50-L103】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L200-L353】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L28-L167】
- `await` expressions bind through a dedicated `BoundAwaitExpression` that validates the `GetAwaiter`/`IsCompleted`/`GetResult` pattern and enforces async-context requirements before capturing the awaiter/result types for downstream phases.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L555-L620】【F:src/Raven.CodeAnalysis/BoundTree/BoundAwaitExpression.cs†L7-L33】
- Accessor binders propagate `async` onto synthesized getter/setter methods, validate that async getters expose task-shaped property/indexer types, and allow async setters to lower through the runtime `AsyncVoidMethodBuilder`. Violations report `AsyncReturnTypeMustBeTaskLike` with the accessor's declared type highlighted for quick remediation.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L888-L944】【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L954-L1023】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L1-L120】

### Lowering and code generation

- `CodeGenerator.EnsureAsyncStateMachines` now walks async methods, local functions, accessors, expression-bodied declarations, and file-scope statements, asks `AsyncLowerer` to analyze their bound bodies, and rewrites anything containing `await`, ensuring a synthesized state machine exists for every async entry point.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L620-L751】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L11-L172】
- The rewrite stage captures the original block, materializes a `SynthesizedAsyncStateMachineTypeSymbol`, and now emits a `MoveNext` that performs `_state` dispatch, hoists each awaiter into a synthesized field, and expands expression-statement awaits into scheduling blocks that set `_state`, call `AwaitUnsafeOnCompleted`/`AwaitOnCompleted`, and resume through labeled continuations that reset `_state = -1` before invoking `GetResult`. Completion now sets `_state = -2` and routes user `return` statements through the method builder’s `SetResult`, forwarding awaited values or expression results so the async contract observes the caller-provided value.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L12-L278】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L41-L107】
- Await lowering walks the async body up front to hoist locals that remain live across awaits into synthesized state-machine fields, rewriting declarations, assignments, and reads to target those fields so resumed execution observes the same values that were in scope before suspension. Await expressions now lower in return values, assignments, arguments, and other expression contexts by expanding to block expressions that stage the awaiter, schedule the continuation, and project the resumed result back into the surrounding expression tree.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L620-L1121】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L245-L417】
- Control-flow constructs such as `if`/`else`, loops, and user `try`/`catch` blocks now rewrite their conditions and bodies through the async lowerer so awaits nested inside branches or exception handlers expand into the same scheduling/resume pattern instead of leaving `BoundAwaitExpression` nodes behind.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L710-L829】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L99-L210】
- Expression-based `try` forms lower through the async rewriter as well, so awaits that appear inside `try` expressions schedule/resume correctly while preserving the surrounding exception flow.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L876-L910】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L385-L417】
- Hoisted locals that require disposal are tracked on the synthesized state machine, appended to the rewritten blocks so they drain at scope exit, and flushed through the `catch` path to preserve `using` semantics even when awaits suspend execution.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L204-L617】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L12-L106】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L368-L417】
- Async method bootstrap instantiates the synthesized struct, copies `this`/parameter values into fields, seeds `_state`, initializes `_builder`, and routes execution through `builder.Start(ref stateMachine)` (falling back to `MoveNext`) before returning the builder’s `Task`, matching the expected runtime handshake. File-scope code reuses the same bootstrap via the cached rewritten block for the implicit `Program.Main`.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L95-L170】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L41-L260】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L150-L226】
- The synthesized type implements `System.Runtime.CompilerServices.IAsyncStateMachine`, exposes a forwarding `SetStateMachine`, and drives the builder’s `SetException` from the provisional `MoveNext` catch block, giving the failure path coverage while the success-path lowering remains to be filled in.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L49-L260】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L174-L233】
- Code generation now materializes synthesized async state machine structs, defines their hoisted fields and builder members, emits the stored `MoveNext`/`SetStateMachine` bodies, and decorates the original async method with `AsyncStateMachineAttribute` and `AsyncMethodBuilderAttribute` so the runtime observes the async metadata.【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L70-L141】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L61-L120】【F:src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs†L161-L239】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1-L121】
- Lambda expressions now accept an optional `async` modifier, allowing awaits inside both simple and parenthesized forms. The binder propagates `IsAsync` onto `SourceLambdaSymbol`, infers `Task<T>` or `Task` return types when no annotation is provided, and ensures lambda bodies convert to the awaited result type before defaulting to builder-friendly tasks.【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/ExpressionSyntaxParser.cs†L393-L521】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceLambdaSymbol.cs†L10-L75】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L12-L279】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLambdaTests.cs†L1-L82】
- Async functions and lambdas surface diagnostics when explicitly annotated with non-`Task` return types so authors receive targeted guidance to use `Task`/`Task<T>` while inference continues to supply the appropriate builder-friendly task shapes.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L295-L329】【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L36-L87】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L119-L187】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L1-L69】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLambdaTests.cs†L1-L120】

### Runtime readiness

- The `SpecialType` enumeration already contains the async method builder types, `Task`, and the attribute metadata we will need, indicating the compilation layer can resolve the required framework symbols once lowering consumes them.【F:src/Raven.CodeAnalysis/SpecialType.cs†L39-L58】
- The sample suite now includes an `async-await.rav` program that compiles through the CLI and executes via `dotnet`, asserting the emitted state machine produces the expected output when awaited operations interleave with synchronous work.【F:src/Raven.Compiler/samples/async-await.rav†L1-L17】【F:test/Raven.CodeAnalysis.Samples.Tests/SampleProgramsTests.cs†L12-L118】

## Remaining work

### Step-by-step plan

1. **Tighten await diagnostics and configurability.** Audit the binder to recognize `await` misuses (e.g., in `lock` statements or query clauses), explore `ConfigureAwait`-style customization, and expand tests to capture the resulting diagnostics.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L555-L620】【F:test/Raven.CodeAnalysis.Tests/Semantics/AwaitExpressionBindingTests.cs†L1-L120】
2. **Broaden runtime validation.** Extend execution coverage beyond the happy-path sample to stress nested awaits, exception resumption, and lambda/accessor scenarios, combining the IL harness with end-to-end runs to lock down observable behaviour.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1-L121】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L1-L417】
3. **Finalize specification and guidance.** Keep the language specification, grammar, and user guidance aligned with the completed async semantics so contributors have authoritative references for the shipped feature set.【F:docs/lang/spec/language-specification.md†L329-L344】【F:docs/lang/spec/grammar.ebnf†L13-L200】

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
