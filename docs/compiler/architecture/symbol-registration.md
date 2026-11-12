# Symbol member registration during the Raven port

`SymbolBase` depends on the internal `SymbolMemberContainer` contract rather than
concrete source and PE namespaces/types. The four existing containers implement
the contract explicitly and forward to their existing `AddMember` methods.
Synchronization, duplicate handling, lazy metadata loading and member order remain
owned by those containers. The contract does not become part of the public symbol API.

Registration retains the original constructor timing: constructors accepting a
kind and name initialize those properties before adding the symbol. Constructors
that opt out do not register; explicit `AddAsMember` remains available internally.
Parameters and type parameters do not register through this path. Types and
namespaces register with a namespace only when they have no containing type.
Locals and labels do not register as type members. A containing type receives
other members through the same contract. The return value of `AddAsMember` retains
its previous meaning, including `true` when no eligible container is present.

The debugger-only merged-namespace marker is a protected virtual property,
overridden by `MergedNamespaceSymbol`. This removes another concrete dependency
from the base without changing the displayed namespace text. PE registration
extension methods live in `PESymbolRegistrationExtensions.cs` and retain their
existing signatures and behavior.

These are shared compiler contracts: both .NET and neoCLR use the same symbol
registration rules. This refactoring introduces no runtime encoding or target
policy, and its modern .NET tests do not establish native neoCLR execution.

## Port validation

The direct Raven `SymbolBase` port uses the same container contract and retains
file-scope access, identity/equality, optional metadata, constructor overloads and
visitor dispatch. Four C# contract tests also run against the isolated emitted
Raven implementation. Source-container integration is tested in the production C# suite.
The port is included in the Raven project, and the four shared tests are linked
into both parity projects. Full-assembly validation remains pending the next
complete build; the running bound-node build predates this addition.

Raven assignments intentionally yield `unit`. The C# expression that initializes
`_fileScopedDeclarationPaths` with `??=` and immediately calls `Add` is translated
into a local value, field assignment and insertion. This preserves lazy allocation
and the null/empty and ordinal-path rules without changing the language's documented
assignment semantics. Property declarations with restricted setters use `val`,
as specified by Raven's property accessibility rules.

Validation: the focused C# symbol baseline passed 220 tests before the change;
the updated suite passes 225, including the five new registration/base cases.
The isolated Raven draft emits without diagnostics and passes the four shared
base-contract cases on .NET 11.
