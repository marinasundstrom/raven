# Bound assignment contracts

Assignment nodes retain reference identity. Their unit result type and right-hand
value are required. The common `BoundAssignmentExpression.Left` property and its
constructor parameter are nullable: a static field assignment uses its absent
receiver as the left operand. Both C# and Raven must describe that intentional
absence in metadata rather than hiding it behind a null-forgiving expression.

Array and indexer assignments retain their more specific, non-null left-access
properties. Walkers visit the receiver/storage before the assigned value, check
optional receivers and left operands, and visit pattern storage as a pattern.
The declaration-side non-null contracts still apply to local and parameter
assignment constructors. Their walker guards also make the nullable base contract
explicit, as required by Raven's stricter null checking.

`BoundAssignmentContractTests` covers static receiver absence, nullable metadata,
update reuse and immutability, and operand order for local, parameter, property, field, array, indexer, member,
pattern and by-reference assignment. These are shared compiler data contracts;
they do not claim execution of emitted assignments on either runtime.

The Raven constructor spells the field parameter `@field` to preserve its metadata
name while distinguishing it from the accessor keyword. This is an existing
language rule, not new assignment syntax. No grammar or editor change is needed.
