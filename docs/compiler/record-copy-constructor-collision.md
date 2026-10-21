# Record primary/copy-constructor collision

A record class primary constructor with a single self-typed parameter conflicts
with its generated copy constructor. Raven now reports RAV0111 at the primary
parameter list, for nullable/nonnullable and generic spellings. Nullable reference
annotations do not distinguish signatures. Explicit copy constructors retain their
existing policy. A recursive record with an additional positional parameter remains
valid. Ordinary structs and other constructor signatures are unaffected.

This follows [C# CS8910](https://learn.microsoft.com/dotnet/csharp/language-reference/compiler-messages/constructor-errors).
The previous native symptom was NEOMETA003 invalid/duplicate method, not a new
platform rule. The fix belongs to shared semantic analysis on main; the neoCLR
line receives it separately. Runtime Contract and metadata ABI are unchanged.

Validation: the 32-test RecordClassSemanticTests baseline and all 37 tests after
the fix pass; new cases check diagnostics and unsuccessful emission, plus the
distinct-signature recursive control.
