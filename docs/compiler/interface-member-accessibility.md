# Interface member accessibility

Source binding preserves all six C# member access levels and public defaults.
Default instance methods and accessors report virtualness through the semantic
symbol API; private helpers are nonvirtual. Invalid private requirements are
reported with `RAV0332` before emission. Implicit implementation matching requires
public contract and implementation members; non-public requirements use explicit
mapping. Reflection emission preserves nonvirtual private bodies instead of
creating private virtual interface slots. These general compiler rules do not
change target capability admission or introduce target-specific access policy.

Regression coverage: `InterfaceMemberAccessibilityTests` checks diagnostics,
semantic symbols, reflected metadata, and explicit dispatch on modern .NET.
This does not establish execution support on .NET Framework, NanoFramework,
or neoCLR. Existing restrictions on static interface features remain separate.

The reference behavior is [C# interface members and implementation mapping](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/interfaces).
The regression matrix also compiles equivalent declarations with Roslyn and
compares CLR method attributes for default instance and static members.
Internal and private-protected virtual members carry `CheckAccessOnOverride`;
combined property/indexer accessor modifiers retain their full access level.
Protected lookup includes inherited interfaces, and explicit implementation
lookup excludes inaccessible helpers.

Validation (2026-10-26): the shared 43-case accessibility matrix passes on both
`main` and `codex/neoclr`. Surrounding filtered suites pass 355 and 404 cases,
respectively, on .NET 11. The neoCLR branch's separate bridge record documents
three broader failures reproduced against its pre-change compiler and excluded
from the final focused run.
