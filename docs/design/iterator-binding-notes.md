# Iterator binding notes

This note documents the current state of iterator (`yield`) support in the binder, lowering pipeline, and backend. It also tracks the remaining work required to finish the generator feature.

## Binder metadata

* `BlockBinder.ResolveIteratorInfoForCurrentMethod` inspects the enclosing method's return type, recognizes the `IEnumerable<T>`, `IEnumerator<T>`, and non-generic iterator shapes, and calls `SourceMethodSymbol.MarkIterator` so the method is flagged as an iterator with a concrete element type. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L569-L618】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L149-L157】
* `BindYieldReturnStatement` and `BindYieldBreakStatement` embed the element type and iterator kind into the bound nodes, applying an implicit conversion to the element type when possible. These nodes therefore carry all metadata the lowerer needs. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L331-L360】

## Lowering entry point and backend integration

* The main lowering pipeline now checks for iterators before performing its usual transformations. Both `LowerBlock` and `LowerStatement` call `RewriteIteratorsIfNeeded`, which invokes `IteratorLowerer` only when the method contains `yield`. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.cs†L22-L37】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.Iterators.cs†L7-L15】
* `IteratorLowerer.Rewrite` determines the iterator signature, marks the method, creates (or reuses) a synthesized state machine via `Compilation.CreateIteratorStateMachine`, and populates the synthesized members and helper bodies. The original method body is rewritten to instantiate the state machine, capture `this` and parameters, seed the initial state, and return the instance. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L24-L281】【F:src/Raven.CodeAnalysis/Compilation.cs†L332-L344】
* During emission the backend ensures all iterator state machines are synthesized and registered before IL generation. `CodeGenerator.EnsureIteratorStateMachines` walks the syntax trees, triggers the iterator rewrite, and the compilation keeps each synthesized iterator in a dedicated cache that `GetSynthesizedIteratorTypes` exposes to the type and method generators. The pass now also visits top-level `func` declarations so script entry points produce their state machines ahead of lowering. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L649-L688】【F:src/Raven.CodeAnalysis/Compilation.cs†L22-L25】【F:src/Raven.CodeAnalysis/Compilation.cs†L332-L344】
* `MethodBodyGenerator.EmitMethodBlock` replays the local declaration pass after lowering so synthesized temporaries introduced by iterator rewriting (for example, the state-machine instance) pick up IL builders even when the rewrite runs on-demand. 【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L513-L569】
* `TypeGenerator.EmitMemberILBodies` now keeps draining un-emitted method generators until none remain, so iterator helpers that materialize while the script body is being lowered are emitted exactly once, matching the class-member pipeline and preventing duplicate IL in their state-machine methods. 【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L394-L406】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L468-L488】
* When composing the rewritten bodies the lowerer now wraps synthesized field writes in `BoundAssignmentStatement` nodes so the statement generator emits them without appending stray `pop` instructions. This keeps the runtime stack balanced and prevents the CLR verifier from rejecting the generated state machines. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L74-L118】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L624-L750】
* The backend only injects an implicit `ret` for iterator helpers when the method's return type is `void`/`unit`, ensuring `MoveNext`/`GetEnumerator` bodies that return `bool` or interface types rely solely on their explicit `return` statements and stay verifiable. 【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L558-L569】

## Synthesized state machine shape

The synthesized iterator type carries the state slot, current element, captured `this`, one field per parameter, and hoisted locals. It also surfaces the expected enumerable/enumerator members—constructor, `Current`, `Dispose`, `Reset`, and `GetEnumerator`—and caches the bound bodies produced during lowering so code generation can emit them. 【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedIteratorTypeSymbol.cs†L17-L199】

`MoveNextBuilder` provides the first pass at state-machine rewriting: it allocates numeric states, injects a dispatch table at the top of `MoveNext`, rewrites each `yield return` into assignments to `_current`/`_state` followed by `return true`, and turns `yield break` into `_state = -1` with `return false`. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L400-L583】

### Comparison with the Roslyn iterator contract

When Roslyn lowers the simple iterator shown below, the generated nested type implements both enumerable and enumerator interfaces, exposes `_state`, `_current`, an `_initialThreadId` field used to guard re-entrancy, and a constructor that accepts the initial state. Every interface member is emitted as an explicit override. 【5a412e†L1-L24】

```
static IEnumerable<int> Test()
{
    int i = 0;
    yield return 42;
    while (i < 2)
    {
        yield return i;
        i++;
    }
}
```

Our synthesized iterator type diverges from that contract in several important ways:

* **Missing thread-id capture.**  We currently allocate only the `_state` and `_current` fields up front, so the state machine never records the creating thread and cannot differentiate the original enumerable instance from clones. 【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedIteratorTypeSymbol.cs†L36-L65】
* **Constructor shape mismatch.**  Roslyn’s iterator constructor accepts the initial state and stores the caller’s thread id, but our constructor is parameterless and never initializes those values. The outer method therefore has to assign `_state` manually after instantiating the state machine. 【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedIteratorTypeSymbol.cs†L252-L274】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L306-L333】
* **Enumerator cloning is unimplemented.**  `CreateGenericGetEnumeratorBody` always resets `_state` to `0` and returns `this`. Roslyn only returns `this` when the caller is iterating on the creating thread and the state is the sentinel `-2`; otherwise it constructs a fresh iterator so multiple `foreach` loops can run independently. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L126-L145】
* **Sentinel states are skipped entirely.**  Because we skip the constructor parameter and hard-code the post-construction assignment to `0`, the state machine never transitions through Roslyn’s `-2` (unstarted enumerable) state. That sentinel is what lets `GetEnumerator` distinguish between the first enumeration and subsequent calls, so its absence prevents us from cloning correctly and makes the generated iterator shape incompatible with the CLR verifier. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L320-L333】

Documenting these gaps clarifies why the emitted type currently fails to load and outlines the work needed to match the CLR-facing contract that C# relies on.

## Local hoisting

Before rewriting the control flow, `MoveNextBuilder` now walks the iterator body to collect every local symbol and assigns each hoistable variable a unique `_localN` field on the synthesized iterator via `SynthesizedIteratorTypeSymbol.AddHoistedLocal`. The builder then rewrites local declarations, reads, and writes so that the state machine persists their values between suspensions, pruning empty declarations and leaving stack-only locals untouched. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L400-L750】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedIteratorTypeSymbol.cs†L123-L132】

## Try/finally scheduling and disposal

`MoveNextBuilder` now tracks the active `try` scopes while rewriting so that `finally` blocks are guarded and replayed when an iterator suspends. Each `BoundTryStatement` with a `finally` is rewritten to wrap the cleanup block in an `if (_state < 0)` guard, preventing the `finally` from running when a `yield return` leaves the method with a positive resume state. The builder snapshots the stack of enclosing `finally` scopes for every resume state and uses it to synthesize a richer `Dispose` body: the generated method resets `_state` to `-1`, executes the guarded `finally` blocks from innermost to outermost, and then returns. This ensures deterministic disposal for suspended iterators and reuses the same `BoundBlockStatement` bodies the runtime executes during normal completion. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L526-L750】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L630-L686】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L697-L750】

## Outstanding work to finish the generator feature

1. **End-to-end validation.**  With the lowering and disposal semantics in place, add integration tests that compile and execute iterator-heavy programs—including script-level functions—to verify the generated state machines behave correctly and that existing lowering paths continue to work. 【F:src/Raven.Compiler/samples/generator.rav†L1-L17】

Updating these notes after each milestone will keep the generator roadmap accurate and highlight any additional issues discovered during implementation.
