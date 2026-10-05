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
Six focused lowering/propagation tests pass both on the isolated fix branch and after
backporting to main, including observable success/failure side effects. No NeoCLR backend
is required. The backport includes only this normalization and its regressions.

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

## Local assignment propagation (2026-10-04)

Shared lowering also expands direct and eager-binary propagation on the right-hand
side of a local assignment. A failure returns before storing the success value or
executing following statements; success evaluates the operand once and performs the
assignment. Local targets have no receiver/index side effects to spill. Property,
field, array and parameter assignments are not extended by this bounded change.

The ordinary .NET backend remains unchanged. Native emission consumes the same
statement-level checks, with no new Runtime Contract configuration or metadata
encoding. Focused lowering coverage checks that the unsupported propagation node is
eliminated; executable .NET tests check success, failure and side effects.

Validation on the isolated main-based branch: 24 focused propagation, Runtime
Contract and async propagation tests pass on .NET 11.


## Conditional propagation initializers (2026-10-04)

Propagation in either branch of a value-producing conditional initializer now lowers
into a statement-level conditional and a shared result temporary. Only the selected
branch executes; its prefix statements run before its propagation operand, and failure
returns without evaluating following operands or statements. Eager binary initializers
retain their existing left-to-right spilling around the conditional.

This extends shared lowering, without target metadata handles or a Runtime Contract
change. Branch blocks with disposal/using declarations are deliberately excluded from
this rewrite until their lifetime can be preserved. Arbitrary argument and receiver
spilling is not part of this change. Ordinary .NET keeps its existing emitter.

Validation: 29 focused lowering, executable propagation, Runtime Contract and async
propagation tests pass on the isolated main-based branch using .NET 11.

## Short-circuit propagation in conditions (2026-10-05)

Propagation now overrides its node visitor, so generated traversal of nested operands
reaches the same rewrite as direct expressions. If conditions with direct/eager-binary
propagation use statement-level temporary values before branching. Logical AND/OR
retain a Boolean temporary and lower the right operand only in the selected branch;
residual returns never bypass a pending left operand. Existing initializer/local
assignment rewriting also benefits from this short-circuit handling.

Eight executable .NET cases check skip/success/failure for both logical operators,
including propagation on either side of a comparison and one-time side effects. A
separate native consumer performs the same assertions against an imported Result
library. HTTP advances from unlowered propagation to a callback-emission capability
gap. This changes shared lowering, not binding, the .NET emitter, Runtime Contract
configuration or metadata. General spilling of all receiver/argument forms remains
outside this bounded change.

Independent main-based validation: 26 propagation/runtime-contract tests pass on
.NET 11. Integration validation: 21 propagation and 65 focused shared-body/runtime-
contract tests pass; the native consumer verifies and exits 0.
