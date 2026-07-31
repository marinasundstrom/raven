# Match statements

Use a match statement when each branch performs an action and the match itself
does not need to produce a value.

Statement-form `match` (`match expr { ... }`) is a control-flow statement. Its
arms are evaluated for effects, and arm values are discarded by default. When a
statement-form `match` is the final statement in a value-returning member, it
is treated as an implicit tail return.

Statement-form `if` follows the same tail rule when it has an `else` branch:
if it is the final statement in a value-returning member (including a
function-expression block body), branch values are treated as an implicit tail
return.

When a statement-form `match` produces values but is not in implicit-return
position, the compiler reports warning `RAV2107`.

An exhaustive match whose arms all leave the current control-flow region does
not have a reachable endpoint. This includes arms that `return` or `throw`,
either directly or as the final operation of an arm block. A match with missing
coverage, a guard that can fail, or any arm that completes normally still has a
reachable endpoint.
