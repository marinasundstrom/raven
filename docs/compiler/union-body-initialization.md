# Synthesized union body initialization

Development, 2026-10-03. The shared synthesized bound bodies now initialize all union
carrier fields before assigning the active case, and assign default to TryGetValue's out
parameter before any failure return. This expresses complete initialization in the compiler
model rather than relying on target allocation details or weakening a verifier.

This is consistent with the .NET union contract. Two plain/generic execution tests exercise
a failed TryGetValue with a previously populated output and observe its default value.
Together with declaration and external-signature tests, 21 focused tests pass. This records
preserved .NET behavior, not a claim that its prior backend failed the same test.

NeoCLR exposed the missing facts because its verifier requires complete value-constructor
and out-parameter initialization on every normal return. No Runtime Contract configuration,
union metadata encoding or new language semantics are introduced by this shared fix.
The compiler's existing Reflection/Emit backend remains in place. Native union publication,
metadata import and broader runtime acceptance are tracked independently.
