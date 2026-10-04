# Propagation temporary lifetime

When a propagation operand has no exception-conversion boundary, lowering binds
its temporary at initialization. In `(await operation)?`, this prevents an empty
carrier from being hoisted before the await has produced it. Operands requiring
exception conversion retain their declaration outside the protected assignment.
No option or precedence changes: `await operation?` applies postfix propagation
before await. The regression executes pending success and completed failure on
ordinary .NET Task; focused protected-await tests validate exception behavior.

## Discarded propagation (2026-10-04)

A statement `_ = expression?` now lowers through the same operand temporary and
residual-return path as a propagation initializer. The operand executes once. Failure
returns before subsequent statements; success does not load the unused output temporary.
Both assignment-statement and expression-statement forms are recognized, but general
pattern assignment and propagation nested inside larger expressions are unchanged.
The existing .NET emitter already supported this source behavior; normalization makes
the lowered body usable by additional emitters without duplicating propagation semantics.
Six focused lowering/propagation tests pass on the main-based compiler-fixes-from-neoclr
branch, including observable success/failure side effects. No NeoCLR backend is required.
