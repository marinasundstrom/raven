# Generalized task-like support (e.g., `ValueTask`)

## Goal
Outline the work needed to generalize Raven's async infrastructure so task-like types (such as `ValueTask` and user-defined awaitables) are treated as first-class citizens alongside `Task`/`Task<T>`. The end state should enable async methods, async entrypoint bridges, and lowering to operate over the full `GetAwaiter` pattern rather than hard-coded task types.

## Current limitations
- Awaitable recognition is limited to `Task`/`Task<T>` in entrypoint validation and async lowering, so types like `ValueTask` produce invalid signature diagnostics even though they are awaitable.
- Async lowering assumes the returned awaitable will be converted to `Task`/`Task<T>` when building state machines, preventing preservation of task-like return types and their performance characteristics.
- Awaiter shape validation largely happens at runtime (reflection in entrypoint bridges) rather than through compile-time checks, leading to silent acceptance of non-awaitable types until code generation fails.

## Step-by-step implementation plan
1. **Inventory awaitable shape checks.**
   - Trace how awaitability is detected across binding, async rewriters, and entrypoint validation; document the helper methods and hard-coded `Task` references that need generalization.
   - Capture where the compiler currently assumes `Task` return types when constructing async state machines or synthesized bridges.
2. **Introduce a shared task-like abstraction.**
   - Define utilities to represent the awaitable pattern (`GetAwaiter` returning an awaiter with `IsCompleted`, `GetResult`, and optional `OnCompleted`/`UnsafeOnCompleted`).
   - Centralize awaitable discovery so async lowering, semantic checks, and bridge emission all consume the same shape information.
3. **Generalize async state-machine lowering.**
   - Allow async method rewriting to preserve the original task-like return type instead of force-converting to `Task`/`Task<T>`.
   - Ensure generated state machines await using the shared awaiter abstraction, including proper result handling and exception propagation for both generic and non-generic task-likes.
   - Update the synthesized builder/awaiter calls (e.g., `AsyncTaskMethodBuilder`) to use task-like-friendly builders or fall back to manually emitting awaiter interactions when no builder exists.
4. **Extend entrypoint signature validation.**
   - Relax `EntryPointSignature` checks to accept task-like return types that satisfy the awaitable pattern, not just `Task`/`Task<int>`.
   - Require an awaitable that returns `int` (or a type convertible to `int`) to produce a console exit code; otherwise, treat the return as `unit`.
   - Emit diagnostics when the awaitable pattern is missing or ambiguous rather than deferring failures to bridge emission.
5. **Refactor bridge emission to use static awaiter knowledge.**
   - Replace reflection-based awaiter discovery with the shared awaitable abstraction so bridge IL generation can statically call `GetAwaiter`/`GetResult` on task-like types.
   - Ensure bridge locals and temporaries handle both struct and reference awaiters without boxing, matching `ValueTask` performance expectations.
6. **Audit library references and metadata flow.**
   - Verify `ValueTask` (and potential user-defined task-likes) are available through reference assemblies and metadata binding so awaiter members resolve correctly.
   - Update symbol lookup/import resolution to surface `System.Runtime` types needed for task-like builders or helper methods.
7. **Update diagnostics and user experience.**
   - Add targeted diagnostics for unsupported awaitable shapes (missing `GetAwaiter`, invalid awaiter members, multiple ambiguous awaiters).
   - Ensure semantic model APIs expose task-like return types and awaitability information for tooling.
8. **Testing and validation strategy.**
   - Expand unit tests to cover async methods and entrypoints returning `ValueTask`, `ValueTask<int>`, and custom awaitables with varying awaiter shapes.
   - Add runtime smoke tests verifying bridge emission awaits task-like types correctly and that `ValueTask<int>` exit codes propagate.
   - Include regression cases for existing `Task` behavior to ensure generalization does not break current semantics.
9. **Documentation and specification updates.**
   - Update the language specification to describe task-like awaitables, including signature rules for async methods and entrypoints.
   - Provide guidance for library authors on implementing compatible awaitable types and how diagnostics surface invalid patterns.
