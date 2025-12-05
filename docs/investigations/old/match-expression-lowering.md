# Lowering strategy for `match` expressions

Raven lowers a bound `match` expression into an expression-bodied block so that control
flow and code generation can rely on more primitive constructs. The block performs the
following steps:

1. **Evaluate the scrutinee once.** The lowerer synthesizes a read-only temporary local
   (named `match_scrutinee`) that captures the incoming scrutinee expression to prevent
   duplicated side effects.
2. **Declare a mutable result slot.** Another synthesized local (`match_result`) stores
   the chosen arm's value. It is initialized to the default for the target type and
   updated when an arm is selected.
3. **Test each arm in order.** For every arm the lowerer rewrites the bound pattern into
   an `is pattern` test against the scrutinee local. Guards are preserved by wrapping the
   arm body inside an `if` statement when present.
4. **Assign the arm value and break.** When an arm matches, its expression is converted to
   the overall match type if required, stored into the result local, and the block jumps
   to a synthesized `match_end` label.
5. **Yield the computed result.** After visiting all arms, the end label is emitted and the
   result local is the final expression value of the block.

This structure ensures that all existing lowering passes and the statement emitter can
operate on `match` expressions without special cases while preserving short-circuiting
semantics and type conversions.
