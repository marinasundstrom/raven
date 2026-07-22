# Match statements

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
