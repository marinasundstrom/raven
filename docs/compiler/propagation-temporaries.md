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

## Eager binary propagation initializers (2026-10-04)

Shared lowering now expands propagation nested in eager binary local initializers,
such as `Left() + Read()? * Right()`, into statement-level checks. It saves each
left operand before evaluating the right operand, including local/field reads that
the right operand may mutate. Failure returns before subsequent operands or
statements execute; success uses the original bound operator and conversions.
This builds on the existing propagation contract, without target metadata handles.

The ordinary .NET emitter already handled these expressions. Shared normalization
lets additional emitters consume them without handling propagation themselves.
Short-circuit operators, propagation within arbitrary invocation arguments and other
expression categories are not extended by this slice; it is not a general expression
spilling pass. Focused tests cover both operand positions, nested arithmetic,
once-only side effects and early failure. No Runtime Contract or metadata change.
