# Async/await investigation

This note tracks the state of Raven's async/await pipeline, the issues currently
blocking parity with C#, and the work required to resolve them.

## Tracking instructions

* Always update the tracking dashboard below so it reflects the current issue
  under active investigation, the upcoming planned step, and any work that has
  been completed.
* When a step or issue finishes, move it to the corresponding "Completed" list
  and promote the next item into the "Current focus" slot.

## Tracking dashboard

### Current focus

* **Issue** – 2. Fix `async Task<T>` entry-point IL (Priority 1)
* **Active step** – Step 8: Adjust async lowering so entry-point state machines
  mutate `_state` in-place before awaiting.

### Upcoming steps

* Step 9: Emit `AsyncTaskMethodBuilder<T>` invocations with constructed
  generics and verify the IL signature requirements.
* Step 10: Promote the console repro into a passing runtime execution test
  once the verifier succeeds.

### Completed steps

* Step 1: Captured the crash stack trace and diagnostics from
  `samples/test8.rav`, confirming that substituted async members crash while
  reflecting against an uncreated `TypeBuilder` instance.【03b865†L1-L64】
* Step 2: Emitted diagnostic trace `RAV9010` for every constructed method
  lookup, recording the definition, containing type, and builder cache status to
  pinpoint which substitutions bypass the cached `MethodBuilder` entries.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L204-L217】【F:src/Raven.CodeAnalysis/Compilation.Emit.cs†L1-L33】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L391-L394】
* Step 3: Designed the builder hand-off so constructed methods first consult
  the `CodeGenerator` cache and project the saved `MethodBuilder` onto the
  substituted `TypeBuilder` before falling back to reflection, covering both
  generic definitions and async state-machine scaffolding.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L27-L52】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L342-L469】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L196-L285】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L360-L470】
* Step 4: Threaded cached `MethodBuilder` handles through
  `ConstructedMethodSymbol.GetMethodInfo`, reusing `TypeBuilder.GetMethod` when
  projecting onto constructed receivers so async substitutions no longer reflect
  over incomplete types.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L42-L67】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L196-L334】
* Step 5: Added `ConstructedAsyncGeneric_EmitsUsingCachedBuilder` to emit a
  constructed async generic end-to-end, asserting the emitted module bytes and
  guarding the instrumentation log so the cached `MethodBuilder` path remains in
  use.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1056-L1126】
* Step 6: Summarised the substituted async builder fix, promoted the entry-point
  IL failure as the next focus, and moved Issue 1 into the completed queue so
  ongoing work tracks the remaining async gaps.
* Step 7: Captured the failing `async Task<int>` entry-point behaviour,
  recorded the CLI crash triggered while printing diagnostics, and mapped the
  lowering work needed to reuse the state-machine address for `_state`,
  builder, and awaiter operations.
* Step 8: Updated `ExpressionGenerator.EmitAddressOfExpression` so value-type
  receivers load `ldarga` when projecting hoisted fields, ensuring
  `AwaitUnsafeOnCompleted`, `SetResult`, and `SetException` receive the
  state-machine by reference rather than by value. The regenerated
  `async_entry.dll` now emits `ldarga.s 0` before the awaiter and builder
  loads.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L880-L915】
  Runtime execution still trips an `AccessViolationException` inside
  `AsyncTaskMethodBuilder<int>.SetException`, so the next step is to trace the
  remaining corruption with the corrected IL in place.【0055cf†L1-L23】

### Completed issues

* **Issue 1 – Unblock substituted async method emission**: Builder reuse now
  projects cached `MethodBuilder` handles onto constructed receivers so async
  substitutions emit without reflecting over uncreated types, and regression
  coverage guards the new lookup path.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L196-L334】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L42-L67】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1056-L1126】

## Prioritized issues

1. **Unblock substituted async method emission** – `ConstructedMethodSymbol.GetMethodInfo`
   reflects over uncreated `TypeBuilder` instances when emitting instantiated async
   members such as `Program.Test(int)`, crashing the compiler before generic async
   methods can be materialised.
2. **Fix `async Task<T>` entry-point IL** – the generated state machine passes the
   struct receiver by value to `AsyncTaskMethodBuilder<T>.AwaitUnsafeOnCompleted`,
   producing unverifiable IL and a `BadImageFormatException` at runtime.
3. **Register async lambda metadata** – nested async state machines still miss
   `CodeGenerator.AddMemberBuilder` registrations for hoisted awaiter fields,
   preventing IL emission for coverage such as
   `AsyncLambda_EmitsStateMachineMetadata`.
4. **Harden lowering and regression coverage** – redundant receiver loads remain
   in complex await scheduling paths, runtime smoke tests are disabled, and the
   documentation/tooling pipeline lacks automated IL verification.

### 1. Unblock substituted async method emission (Priority 0)

**Impact** – `samples/test8.rav` and any async method that materialises a
constructed generic currently fail to emit because the compiler crashes before it
can hand out `MethodInfo` handles for instantiated members.

**Current behaviour** – running
`dotnet run --project src/Raven.Compiler/Raven.Compiler.csproj -- src/Raven.Compiler/samples/test8.rav -o test.dll -d pretty`
throws `System.NotSupportedException: The invoked member is not supported before
the type is created.` while lowering the async state machine, leaving `test.dll`
unproduced. 【c6da48†L1-L11】【2b0969†L1-L47】

**Investigation summary** – instrumentation confirms the emitter successfully
resolves `Create`, `Start`, `AwaitUnsafeOnCompleted`, and `SetResult` on
`AsyncTaskMethodBuilder<T>` before attempting to instantiate the substituted
`Program.Test(int)` backing method. Because the containing `Program` type still
has an uncreated `TypeBuilder`, enumerating `methodSearchType.GetMethods` inside
`ConstructedMethodSymbol.GetMethodInfo` throws `TypeBuilderImpl.ThrowIfNotCreated`.
The crash is therefore isolated to the substitution lookup rather than to the
async builder handshake itself. 【5406d5†L65-L132】【d5ec68†L1-L36】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L201-L270】

**Proposed fix** – teach `ConstructedMethodSymbol` (and callers such as
`SubstitutedMethodSymbol`) to reuse the `MethodBuilder` handles recorded through
`CodeGenerator.AddMemberBuilder` instead of reflecting over incomplete
`TypeBuilder` instances. This keeps async emission on the Reflection.Emit path and
lets `samples/test8.rav` complete successfully. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L18-L61】

**Step-by-step plan**

1. **Document the crash behaviour** – capture the failing stack trace and
   diagnostics from the `test8.rav` sample and paste them into the investigation
   log so regressions can be detected quickly. (Status: _Complete_.【03b865†L1-L64】)
2. **Audit substitution call sites** – add temporary diagnostics in
   `ConstructedMethodSymbol.GetMethodInfo` to log the constructed method symbol,
   the containing definition, and whether a `MethodBuilder` entry already exists
   in `CodeGenerator.AddMemberBuilder`. This verifies which lookup path misses the
   cache. (Status: _Complete_.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L204-L217】)
3. **Design the caching hand-off** – map the data flow from
   `CodeGenerator.AddMemberBuilder` through `TypeGenerator` and confirm where the
   constructed method should look up the original `MethodBuilder`. Ensure the plan
   covers both generic methods and async lambdas lowered through substitution.
   (Status: _Complete_.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L27-L52】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L342-L469】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L196-L285】)
4. **Implement builder reuse** – thread the cached `MethodBuilder` through the
   substitution path, updating `ConstructedMethodSymbol` and any helpers so they
   return the cached handle without touching `TypeBuilder.GetMethods`. Keep the
   Reflection.Emit path covered with targeted unit tests. (Status:
   _Complete_.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L42-L67】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L196-L334】)
5. **Add regression coverage** – extend the `samples/test8.rav` scenario (or add a
   dedicated test) to assert that emitting a constructed async generic produces IL
   successfully, and include a guard that fails if `TypeBuilder` reflection is
   attempted before creation. (Status:
   _Complete_.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1056-L1126】)
6. **Update documentation and tracking** – summarise the fix in this note, move
   the completed steps into the "Completed steps" list above, and adjust the
   upcoming focus to the next priority issue. (Status: _Complete_.)

#### Issue 1 resolution summary

* `ConstructedMethodSymbol.GetMethodInfo` now rehydrates cached builders for
  substituted async members before instantiating generics, so emission never
  reflects over incomplete `TypeBuilder` instances.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L19
6-L334】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L42-L67】
* `ConstructedAsyncGeneric_EmitsUsingCachedBuilder` keeps the regression
  reproducible by asserting the cached `MethodBuilder` path and the emitted
  module bytes for the `samples/test8.rav` scenario.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1056-L11
26】

#### Step 1 crash log

```
Unhandled exception. System.NotSupportedException: The invoked member is not
supported before the type is created.
   at System.Reflection.Emit.TypeBuilderImpl.ThrowIfNotCreated()
   at System.Reflection.Emit.TypeBuilderImpl.GetMethods(BindingFlags bindingAttr)
   at Raven.CodeAnalysis.Symbols.ConstructedMethodSymbol.GetMethodInfo(CodeGenerator codeGen) in
       /workspace/raven/src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs:line 219
   at Raven.CodeAnalysis.MethodSymbolExtensionsForCodeGen.GetClrMethodInfo(IMethodSymbol methodSymbol, CodeGenerator codeGen) in
       /workspace/raven/src/Raven.CodeAnalysis/MethodSymbolExtensionsForCodeGen.cs:line 34
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.GetMethodInfo(IMethodSymbol methodSymbol) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 3243
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.EmitInvocationExpressionBase(BoundInvocationExpression invocationExpression,
       Boolean receiverAlreadyLoaded) in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 2651
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.EmitInvocationExpression(BoundInvocationExpression invocationExpression,
       Boolean receiverAlreadyLoaded) in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 2533
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.EmitExpression(BoundExpression expression) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 85
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.EmitInvocationExpressionBase(BoundInvocationExpression invocationExpression,
       Boolean receiverAlreadyLoaded) in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 2570
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.EmitInvocationExpression(BoundInvocationExpression invocationExpression,
       Boolean receiverAlreadyLoaded) in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 2533
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.EmitExpression(BoundExpression expression) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 85
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.EmitAssignmentExpression(BoundAssignmentExpression node, Boolean preserveResult)
       in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 1707
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.Emit() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 38
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.EmitAssignmentStatement(BoundAssignmentStatement assignmentStatement) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 245
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.Emit() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 40
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.EmitStatement(BoundStatement statement, Scope scope) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 3233
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.EmitBlock(BoundBlockExpression block) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 3095
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.EmitExpression(BoundExpression expression) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 114
   at Raven.CodeAnalysis.CodeGen.ExpressionGenerator.Emit() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs:line 42
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.EmitDeclarator(BoundLocalDeclarationStatement localDeclarationStatement,
       BoundVariableDeclarator declarator) in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 615
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.EmitDeclarationStatement(BoundLocalDeclarationStatement localDeclarationStatement) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 604
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.Emit() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 44
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.EmitBlockStatement(BoundBlockStatement blockStatement) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 527
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.Emit() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 60
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.EmitLabeledStatement(BoundLabeledStatement labeledStatement) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 542
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.Emit() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 64
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.EmitBlockStatement(BoundBlockStatement blockStatement) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 527
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.Emit() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 60
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.EmitTryStatement(BoundTryStatement tryStatement) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 481
   at Raven.CodeAnalysis.CodeGen.StatementGenerator.Emit() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs:line 56
   at Raven.CodeAnalysis.CodeGen.MethodBodyGenerator.EmitBlock(BoundBlockStatement block, Boolean treatAsMethodBody, Boolean includeImplicitReturn) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs:line 689
   at Raven.CodeAnalysis.CodeGen.MethodBodyGenerator.EmitMethodBlock(BoundBlockStatement block, Boolean includeImplicitReturn) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs:line 651
   at Raven.CodeAnalysis.CodeGen.MethodBodyGenerator.EmitAsyncStateMachineMethod(SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine) in
       /workspace/raven/src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs:line 407
   at Raven.CodeAnalysis.CodeGen.MethodBodyGenerator.Emit() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs:line 124
   at Raven.CodeAnalysis.CodeGen.MethodGenerator.EmitBody() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs:line 438
   at Raven.CodeAnalysis.CodeGen.TypeGenerator.EmitMemberILBodies() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs:line 518
   at Raven.CodeAnalysis.CodeGen.CodeGenerator.EmitMemberILBodies() in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs:line 1032
   at Raven.CodeAnalysis.CodeGen.CodeGenerator.Emit(Stream peStream, Stream pdbStream) in /workspace/raven/src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs:line 386
   at Raven.CodeAnalysis.Compilation.Emit(Stream peStream, Stream pdbStream) in /workspace/raven/src/Raven.CodeAnalysis/Compilation.Emit.cs:line 19
   at Program.<Main>$(String[] args) in /workspace/raven/src/Raven.Compiler/Program.cs:line 234
```

The stack trace above was captured by running
`ravenc` against `samples/test8.rav` with pretty diagnostics enabled, matching
the repro described in the step plan.【03b865†L1-L64】

#### Step 3 caching hand-off design

1. **Surface the cached builders before reflection** – expose a
   `CodeGenerator.TryGetMemberBuilder` helper so constructed methods can query the
    `SourceSymbol → MemberInfo` map without throwing. The map already tracks every
    source method, property accessor, and synthesized async member when
    `TypeGenerator.DefineMemberBuilders` runs, so the constructed lookup can reuse
   the matching `MethodBuilder` or `MethodInfo` instead of enumerating
   `TypeBuilder.GetMethods` prematurely.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L27-L52】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L342-L469】
2. **Project builders onto substituted types** – teach
   `ConstructedMethodSymbol.GetMethodInfo` to request the cached builder by
   walking back to the source definition via `TryGetSourceDefinitionSymbol`, then
   use `TypeBuilder.GetMethod` to materialise the instantiated handle when the
   containing runtime type is a `TypeBuilder` or a `TypeBuilderInstantiation` for a
   generic async method. This mirrors the existing substituted-type logic and
   keeps the Reflection.Emit path alive even before the type is created.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L196-L285】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L360-L470】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L360-L365】
3. **Fallback for external metadata** – retain the current reflection search
   when the definition originates from metadata (`PEMethodSymbol`) or when no
   builder was registered (e.g., unsupported synthesized shapes), ensuring the new
   path is additive and debuggable via the existing `RAV9010` diagnostics.
4. **Verification plan** – rerun `ravenc` on `samples/test8.rav` to confirm the
   crash disappears once builder reuse is active and extend coverage so a failing
   test asserts that the substitution path no longer touches `TypeBuilder.GetMethods`.
   The current failure reproduces consistently and keeps the need for the change
   visible during development.【e3a4ac†L1-L47】

#### Step 4 builder reuse implementation

* **Cache lookup helper** – Added `CodeGenerator.TryGetMemberBuilder` so
  constructed methods can detect whether a cached `MemberInfo` exists without
  triggering exception paths when the cache misses.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L42-L67】
* **Projection without reflection** – Updated
  `ConstructedMethodSymbol.GetMethodInfo` to fetch the cached `MethodInfo`,
  project it onto substituted receivers with `TypeBuilder.GetMethod`, and
  instantiate generic method arguments before falling back to reflection.
  This keeps async substitutions on the cached Reflection.Emit path and avoids
  premature `TypeBuilder.GetMethods` calls that previously crashed the build.
  【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L196-L334】

#### Step 5 regression coverage

* **Constructed generics emit successfully** –
  `ConstructedAsyncGeneric_EmitsUsingCachedBuilder` compiles the core of
  `samples/test8.rav` through `CodeGenerator.Emit` and asserts both a non-empty
  PE stream and the availability of the cached definition `MethodBuilder`,
  keeping the async substitution path on the Reflection.Emit surface before type
  creation.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1056-L1109】
* **Guard against premature reflection** – the regression test invokes
  `ConstructedMethodSymbol.GetMethodInfo` directly and inspects the `RAV9010`
  instrumentation, failing if the `Program.Test` lookup reports a cache miss so
  future changes cannot regress to `TypeBuilder.GetMethods` before creation.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1100-L1126】

#### Step 7 entry-point remediation plan

1. **Reproduce the failing entry-point** – target
   `AsyncEntryPoint_WithTaskOfInt_ExecutesSuccessfully` so the compiled
   `Program.Main` bridge runs inside the test harness, capturing the null return
   value that proves the awaited result never flows back to the caller.【ef929a†L10-L23】
2. **Unblock CLI-driven validation** – harden `ConsoleEx.PrintDiagnosticList`
   so diagnostics with unresolved paths render without throwing, allowing the
   CLI repro to execute end-to-end and surface the current runtime failure in a
   controllable environment.【959dee†L1-L23】【F:src/Raven.Compiler/ConsoleEx.cs†L110-L139】
3. **Audit `MainAsync` lowering** – extend `CaptureAsyncInstructions` coverage to
   read the `Program.MainAsync` state machine, confirming where `_state`,
   `_builder`, and awaiter locals lose the struct address before awaiting. This
   informs the precise rewrites required in `AsyncLowerer` and
   `SynthesizedMainAsyncMethodSymbol` to preserve by-ref receivers.
4. **Define verification guardrails** – once lowering is corrected, promote the
   CLI scenario into a runtime execution test alongside the in-memory harness so
   both the synchronous bridge and the emitted assembly return the awaited
   `int`, and extend IL assertions to ensure `AwaitUnsafeOnCompleted` consumes a
   managed pointer to the state machine.

### 2. Fix `async Task<T>` entry-point IL (Priority 1)

**Impact** – the runtime rejects the generated assembly for `async Task<T>` entry
points, so even simple `await Task.FromResult(42)` programs fail to execute.

**Current behaviour** –
`AsyncEntryPoint_WithTaskOfInt_ExecutesSuccessfully` currently fails because the
generated `Program.Main` reflection bridge returns `null` instead of the awaited
`int` result, showing that the state machine never commits the value produced by
`MainAsync`. 【ef929a†L10-L23】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L452-L520】

**Evidence** – attempting to execute the emitted assembly through the CLI exits
with code 134 after `ravenc` crashes while formatting diagnostics for the
`async Task<int>` sample, revealing that the harness cannot currently promote
the repro into a runnable scenario. This blocks runtime validation until the
diagnostic printer tolerates source-less locations. 【959dee†L1-L23】【F:src/Raven.Compiler/ConsoleEx.cs†L110-L139】

**Proposed fix** – update async lowering so `_state`, `_builder`, and hoisted
awaiters share a single receiver load (`ldarg.0`/`ldarga`) and ensure every
`AsyncTaskMethodBuilder<T>` call honours `RequiresReceiverAddress`. Once the IL
matches Roslyn’s pattern, convert the console repro into a passing runtime test
that validates the awaited value flows through the entry point.

**Next steps**

* Adjust the lowering shape to avoid copying the struct before mutating `_state`.
* Emit `AsyncTaskMethodBuilder<T>` invocations using the constructed generic type.
* Promote the repro into a runtime execution test once the verifier passes.

**Latest progress**

* Added `ContainsAwaitInitializerOutsideNestedFunctions` so top-level locals that
  assign the result of an `await` expression force the synthesized `MainAsync` to
  return `Task<int>`, matching Roslyn's entry-point heuristics.
  【F:src/Raven.CodeAnalysis/Compilation.cs†L247-L355】
* Hardened `Compilation.EnsureSetup` with an `_setupInProgress` guard so the new
  syntax walk does not recurse into `Setup()` while metadata is still loading;
  the stack overflow observed by the test harness no longer reproduces.
  【F:src/Raven.CodeAnalysis/Compilation.cs†L148-L203】
* `AsyncEntryPoint_WithTaskOfInt_ExecutesSuccessfully` now reaches IL emission
  but still fails at runtime with a `NullReferenceException`, showing the awaited
  value is not yet committed back through `Program.Main`.
  【8ac73e†L5-L17】
* Regression surfaced: the `_setupInProgress` short-circuit means `DetermineBuilderType`
  observes `ErrorTypeSymbol` while `Setup()` is still loading metadata, so
  `CreateReturnExpression` synthesizes a `null` task and the generated `Main` bridge
  throws `NullReferenceException` when it awaits the entry point.
  【F:src/Raven.CodeAnalysis/Compilation.cs†L148-L203】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L179-L213】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1771-L1794】【92a067†L1-L17】

### 3. Register async lambda metadata (Priority 2)

**Impact** – nested async state machines cannot be emitted reliably, blocking
coverage that asserts metadata for hoisted awaiters and nested `MoveNext`
implementations. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L667-L711】

**Current behaviour** – lambda state machines miss `CodeGenerator.AddMemberBuilder`
registrations for hoisted awaiter fields, so emission fails when IL generation
tries to resolve the missing handles. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L17-L47】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L560-L623】

**Proposed fix** – mirror the method-level registration path by recording each
lambda state machine’s `_state`, `_builder`, and hoisted awaiter fields before IL
emission. Extend the closure factory so nested `MoveNext` bodies know about the
awaiter and hoisted-local fields they need to access.

**Next steps**

* Register lambda field builders alongside captured variables.
* Extend regression coverage to assert metadata exists for async lambdas and
  nested functions once registration succeeds.

### 4. Harden lowering and regression coverage (Priority 3)

**Impact** – even after the primary crashes are fixed, the remaining gaps in
lowering and tooling leave the async pipeline brittle and difficult to verify.

**Current behaviour** – redundant receiver loads still appear in complex await
scheduling paths, runtime smoke tests such as the minimal
`await Task.CompletedTask` program remain disabled, and developers must run
`ilverify`/`peverify` manually to validate IL. The current workflow also requires
manual generator refreshes before the compiler builds.

**Proposed fix** –

* Consolidate await scheduling so receiver loads are shared across `_state` and
  awaiter stores, mirroring Roslyn’s `ldarg.0`, `dup`, `stfld` sequence.
* Restore runtime execution coverage for minimal async programs and
  `samples/async-await.rav` once the IL is fixed.
* Integrate IL verification into the CLI (e.g. `ravenc --ilverify`) and wire it
  into CI to catch regressions automatically.
* Document the generator prerequisites so contributors can run async tests without
  missing codegen steps.

## Supporting context

### Current implementation snapshot

**Language surface area**

* `async` is accepted on top-level functions, members, lambdas, and accessors;
  parsing keeps the modifier attached so the binder can flow `IsAsync` through the
  pipeline.
* Async declarations without an explicit return type default to
  `System.Threading.Tasks.Task`, and `async func Do() -> Task<Unit>` is treated as
  sugar for `async func Do() -> Task` so entry-point expectations continue to
  mirror C# while still emitting `System.Threading.Tasks.Task` metadata.
* The non-generic `System.Threading.Tasks.Task` surface is projected as
  `Task<Unit>` in Raven to mirror the language's explicit `Unit` value; the
  emitter erases that projection back to the CLR `Task` so runtime metadata
  remains source-compatible with existing async consumers.
* Global statements lower to an async `Program.MainAsync` that returns `Task` or
  `Task<int>` while `Program.Main` synchronously bridges to the awaited result.

**Binding and lowering**

* `BoundAwaitExpression` validates the awaited pattern and captures awaiter and
  result types; async binders mark rewritten bodies so the lowerer can generate
  state machines.
* `AsyncLowerer` hoists locals that survive across awaits, rewrites control-flow
  constructs, and produces a `SynthesizedAsyncStateMachineTypeSymbol` whose
  `MoveNext` implements the expected `_state` dispatch and resume paths.
* Disposal and expression-bodied members flow through the same rewriter, keeping
  async `using` patterns and lambda lowering aligned with synchronous code paths.

**Emission**

* Code generation creates the synthesized struct, emits `MoveNext`/`SetStateMachine`,
  and annotates async methods with the appropriate builder attributes so the CLR
  observes the async metadata.
* The bootstrap path instantiates the struct, seeds `_state = -1`, initializes the
  builder, and returns the builder task, matching the Roslyn handshake for both
  declaration bodies and global statements.
* Builders and hoisted awaiters now use `TryEmitValueTypeReceiverAddress`, keeping
  `_state`, `_builder`, and awaiter fields operating on the in-place struct so
  mutations do not spill through temporaries.
* Synthesized async state machines reuse `EnsureFieldBuilder` when materialising
  `_state`, `_builder`, and hoisted-awaiters so emission no longer defines the
  same field twice before IL generation resolves the handle.
  【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L243-L399】
* Taking the address of the state machine now always pushes `ldarg.0`, ensuring
  builder calls receive a managed pointer to the struct rather than a by-value
  copy when the receiver is omitted.
  【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L905-L926】

### Backlog aligned with the priority list

1. **Codify desired semantics for `Task<T>` async methods** – document runtime
   expectations (exception flow, return propagation, synchronization context) and
   align Raven's specification with Roslyn’s lowering strategy.
2. **Extend binder and symbol shape** – track constructed `Task<T>` return types
   on async methods and lambdas, including default inference and diagnostic
   coverage for mismatches.
3. **Refine async lowering** – share receiver loads across `_state` and hoisted
   awaiters, emit constructed generic builders, and honour `RequiresReceiverAddress`
   for every builder call.
4. **Adjust code generation** – produce correctly constructed generic builder
   fields and metadata handles, and reuse cached `MethodBuilder` instances for
   substituted methods so emission no longer reflects over uncreated types.
5. **Regression and conformance testing** – flip existing failing tests to assert
   successful execution, add IL baselines covering builder construction and
   awaited value flow, and promote runtime smoke tests that await `Task<T>` entry
   points.
6. **Tooling and documentation** – wire IL verification into the CLI/CI workflow,
   capture generator prerequisites, and document async debugging guidance.
7. **Harden nested async state machines** – register lambda field builders before
   IL generation, describe hoisted awaiters in the closure factory, and extend
   regression coverage accordingly.

## Open items

* Fix async lambda emission so hoisted awaiters register their field builders
  before IL generation; this unblocks
  `AsyncLambda_EmitsStateMachineMetadata` and exercises the nested-state-machine
  coverage described in Priority 2. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L17-L47】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L560-L623】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L667-L711】
* Revisit await scheduling heuristics to eliminate the redundant receiver loads
  that still show up in IL when lowering complex control-flow (Priority 3).
* Restore runtime execution coverage by fixing the minimal `await Task.CompletedTask`
  program and the `samples/async-await.rav` regression so smoke tests can assert
  the generated state machines reach completion.
* Integrate the new `ravenc --ilverify` switch (or `peverify`) into CI once the
  verification tooling is stable.
