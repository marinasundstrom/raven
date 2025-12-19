# Async entrypoints for `Program.Main` and `func Main`

## Goal
Capture what is needed to support async entrypoints declared as `Program.Main` methods and `func Main` functions. The intent is to mirror top-level async synthesis (which lowers to a sync bridge) while handling the signature rules, lowering, and diagnostics for explicit entrypoint declarations.

## Current behavior
- `EntryPointSignature` accepts `Task` and `Task<int>` return types, but async `Main` declarations rely on a synthesized bridge (`<Main>_EntryPoint`) only when the candidate is already async (typically from top-level lowering). `SynthesizedEntryPointBridgeMethodSymbol` currently assumes the async implementation is a `SourceMethodSymbol` and emits `GetAwaiter`/`GetResult` calls directly.
- Top-level programs synthesize a synchronous `Main` plus an async implementation (`MainAsync`) when needed, so async is implicitly supported there. For user-authored `Program.Main` and `func Main`, the compiler appears to surface the async method directly as the entrypoint unless a bridge is synthesized.
- Diagnostics for entrypoint resolution live in `Compilation.EntryPoint` and expect synchronous return types for console apps unless the bridge is created. There is no explicit diagnostic for async `Main` lacking a sync wrapper.

## Investigation plan
Mark completed steps explicitly as they are finished to keep progress visible.

1. ✅ **Map entrypoint discovery and lowering for explicit mains.**
   - `Compilation.EnsureEntryPointComputed` walks `SourceGlobalNamespace.GetAllMembersRecursive()` and filters with `IsEntryPointCandidate`, so both `Program.Main` and `func Main` surface via the same `EntryPointSignature` checks as long as they appear as methods in the source namespace. Unique candidates are keyed by `SymbolDisplayFormat.CSharpSymbolKeyFormat` before ambiguity diagnostics are issued.
   - For console apps, the entrypoint is either the lone candidate or a synthesized bridge via `TrySynthesizeAsyncEntryPointBridge`. Non-console output kinds skip bridge synthesis entirely and only accept a single candidate.
   - Bridge synthesis is bypassed when the candidate is a `SynthesizedMainMethodSymbol` that already has an async implementation (top-level async lowering) or when the containing symbol is not a `SourceNamedTypeSymbol`. As a result, `func Main` must emit as a source method on a source type to gain a bridge; otherwise the async path is abandoned silently.
   - When a bridge is created, it is added as a member of the containing type and returned as the chosen entrypoint.
2. ✅ **Document current codegen path for async bridges.**
   - `MethodBodyGenerator.EmitCore` dispatches to `EmitEntryPointBridge` for both `SynthesizedMainMethodSymbol` (top-level async) and `SynthesizedEntryPointBridgeMethodSymbol` before any other lowering logic runs.
   - `SynthesizedEntryPointBridgeMethodSymbol` clones parameters from the async implementation (if any) into the wrapper and stamps `returnType` using `EntryPointSignature.ResolveReturnType`, but the wrapper is always static/implicit.
   - `EmitEntryPointBridge` requires the async implementation to be a `SourceSymbol` with a `MethodInfo` produced by `TypeGenerator.CodeGen.GetMemberBuilder`; otherwise it throws `NotSupportedException`, so non-source implementations (or other pipeline-produced methods) cannot be awaited today.
   - The bridge simply forwards args, calls the async implementation, then reflects `GetAwaiter`/`GetResult` to synchronously block. It pops non-void results when the bridge returns `Unit` and does not translate exit codes; async `Task<int>` relies on `GetResult` being the int exit code.
3. ✅ **Survey top-level async synthesis as a reference.**
   - Top-level programs are scanned per compilation unit: `requiresAsync` is set when any global statement (excluding nested functions/lambdas) contains `await`, triggering creation of a `SynthesizedMainAsyncMethodSymbol` alongside the sync `SynthesizedMainMethodSymbol`. `returnsInt` is tracked separately and flows to both symbols when a top-level return expression is present.
   - Both `Main` and `MainAsync` are implicitly declared, static, and parameterized with a single `string[] args` that is synthesized via `EntryPointSignature.CreateStringArrayType`. `MainAsync` uses `EntryPointSignature.ResolveAsyncReturnType` to select `Task` or `Task<int>`.
   - Lowering rewrites the collected global statements into a `BoundBlockStatement` and runs `AsyncLowerer.Rewrite` on the synthesized `Main` when `ShouldRewrite` reports async work. This caches an async state machine for the compilation unit, mirroring normal method async rewriting and ensuring bridge emission receives a full async body.
4. ✅ **Design signature rules for async declared mains.**
   - Accept `Task` and `Task<int>` as the baseline async return types for both `Program.Main` and `func Main`, mirroring top-level synthesis and allowing bridge emission to preserve exit codes (`Task<int>`). Continue to allow synchronous `Unit`/`int` return types but route them through the async bridge path only when they are truly async (e.g., an async method returning `Task<int>`).
   - Permit either a parameterless signature or a single `string[] args` parameter; the bridge should clone whichever shape is present and preserve defaults/`params` if allowed today.
   - To keep Raven future-proof, generalize `EntryPointSignature.IsAsyncReturnType` to recognize task-like types (per `GetAwaiter` pattern) rather than hard-coding `Task`/`Task<int>`. Even if `ValueTask` is not used by `Main`, the bridge and diagnostics should leverage the same task-like detection to avoid diverging behavior once task-like support lands elsewhere in the compiler.
5. ✅ **Plan lowering strategy.**
   - Prefer the existing bridge pattern: keep the user-authored async method intact and synthesize a synchronous `<Main>_EntryPoint` wrapper that forwards parameters, awaits via a generalized awaiter helper (so task-like implementations such as `ValueTask` can plug in later), and translates `Task<int>` results into exit codes.
   - Avoid rewriting the declared method body to be synchronous; doing so would duplicate async lowering/state-machine generation and diverge from user expectations about method identity and debugging. Instead, reuse the lowered async body produced during normal async rewriting and have the bridge block on the awaiter.
   - Ensure the wrapper is emitted in the same containing type so metadata entrypoint resolution matches the sync bridge while the async implementation remains discoverable for tooling.
6. ✅ **Define diagnostic surface.**
   - Async entrypoints in console apps should trigger bridge synthesis; if a bridge cannot be produced (unsupported return type/parameters/output kind), surface a diagnostic rather than silently accepting the async method as the entrypoint. This distinguishes console apps (which expect sync metadata entrypoints) from library/windows kinds (which can keep the async method but should still report unsupported signatures).
   - Reuse the existing `EntryPointSignature`/`Compilation.EntryPoint` diagnostics where possible (ambiguous entrypoints, multiple candidates, wrong return types) and add a targeted diagnostic for "async entrypoint requires synchronous wrapper" to flag non-awaitable return types or missing awaiter pattern.
   - When async return types are not awaitable (per `GetAwaiter` shape), report a specific error instead of falling back to synchronous handling. Bad parameter lists should reuse the current `MainParameterInvalid` pattern but mention async context if relevant.
7. ✅ **Assess refactoring needs.**
   - Factor awaiter acquisition and bridge emission into shared helpers used by both top-level async wrappers and declared-main bridges, eliminating the current `SourceMethodSymbol`/reflection dependency and enabling task-like support without duplicating lowering.
   - Keep state-machine generation unchanged: generalized task-like awaiter support should be handled in the bridge/wrapper emission path, not by rewriting async methods differently. This keeps task-like generalization scoped to awaiting, not to how async state machines are produced.
   - Consider a lightweight abstraction (interface or utility) for parameter cloning and return-type translation (`Task<int>` exit codes) so future entrypoint forms reuse consistent logic.
8. ✅ **Validation plan.**
   - Compiler diagnostics: async `Program.Main`/`func Main` returning `Task`, `Task<int>`, `ValueTask`-like, invalid return types (e.g., `Task<string>`), invalid parameters (`int x`, extra params), and non-console output kinds (ensure bridge is skipped or diagnostic issued per design).
   - Codegen/runtime: bridge emission for parameterless and `string[] args` async mains; ensure `Task<int>` exit codes propagate; verify bridge awaits task-like types detected via `GetAwaiter` pattern; confirm non-void results are discarded correctly for `Unit` bridge returns.
   - Regression: top-level async programs remain unchanged; synchronous mains keep current behavior. Add minimal runtime smoke tests to ensure awaited completion and exit codes, but exclude generalized state-machine generation for task-like types (out of scope for async entrypoints).
9. ✅ **Inspect bridge IL emission constraints.**
   - `MethodBodyGenerator.EmitCore` dispatches to `EmitEntryPointBridge` for both synthesized top-level wrappers and declared `Main` bridges, so the entrypoint wrapper body is fully synthesized in codegen (not bound from syntax).
   - `EmitEntryPointBridge` currently assumes the async implementation is a `SourceSymbol` with an available `MethodInfo` and uses reflection to find `GetAwaiter`/`GetResult` on the runtime type. This means bridge emission silently falls back to runtime failures when the awaiter pattern is absent and cannot currently await non-source implementations.
   - Awaiter acquisition and result handling happen directly in IL (load args → call async → call `GetAwaiter` → stash awaiter local → `GetResult` → pop if `Unit`). There is no exit-code translation beyond relying on `Task<int>.GetResult`, and no static validation that the awaited type follows the awaiter pattern.

## Re-evaluated implementation plan
- Keep discovery predictable
  - Ensure bridge synthesis is available for both `Program.Main` and `func Main` as long as a source method lands in a source type; otherwise leave the entrypoint unresolved with diagnostics rather than returning the async method directly.
  - Maintain current output-kind behavior (bridge for non-library kinds) but emit clear diagnostics when async signatures are unsupported instead of depending on runtime failures.
- Constrain signature scope for now
  - Support `Task` and `Task<int>` entrypoints only; treat `ValueTask`/task-like return types as unsupported until broader awaitable work lands. The async-return checker should reflect that limitation to avoid accidental bridge synthesis.
  - Continue to permit either no parameters or a single `string[] args`, cloning the shape into the bridge and preserving static/implicit metadata flags.
- Refactor bridge emission for safety
  - Replace reflection-based awaiter discovery in `EmitEntryPointBridge` with the same awaiter-lowering used by top-level async wrappers so awaitable support is validated and centralized.
  - Add static checks on the awaited return type before emitting IL (diagnostic when `GetAwaiter`/`GetResult` are missing) and keep exit-code translation as the `Task<int>` `GetResult` value for now.
  - Decouple bridge emission from `SourceSymbol`/`MethodInfo` assumptions so the async implementation can come from other pipelines without throwing `NotSupportedException` during codegen.
- Testing and validation
  - Expand diagnostics coverage to assert that unsupported awaitable shapes (e.g., `ValueTask`) and missing awaiter patterns report errors instead of silently selecting the async method.
  - Add runtime smoke tests for `Task`/`Task<int>` bridges (both parameterless and `string[] args`) to prove the wrapper blocks, awaits, and returns the correct exit code; keep task-like/state-machine generalization explicitly out of scope for this phase.

## Implementation progress
- ✅ Entry point bridge emission now relies on shared awaitable-pattern detection and symbol-based call emission, removing reflection dependencies while keeping support scoped to `Task`/`Task<int>`.
- ✅ Bridge synthesis deduplicates `<Main>_EntryPoint` members on the containing type and emits void-returning awaiters without popping the stack, fixing missing bodies and invalid IL for async console entrypoints.

## Open questions
- `ValueTask`/task-like support is deferred to a future awaitable generalization pass; the current phase should reject these shapes explicitly.
- Are there language semantics differences between `Program.Main` and `func Main` that require separate lowering pipelines?
- How should async entrypoints behave for non-console output kinds (e.g., library or windows application) where no bridge is emitted today?
