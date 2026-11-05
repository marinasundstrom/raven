# Constructor initializer binding

Explicit instance constructor initializers are resolved after source class member
registration. Signature registration records the initializer syntax but does not
run overload resolution against a partially declared base. The completion phase
resolves arguments in the declaring constructor's parameter scope, records the bound
call on that source symbol and reports failures before emission. Constructor binder
re-entry reuses the declaration's symbol rather than publishing a second symbol for
the same owner, syntax tree and span. Local and nested classes use the same phase.

This fixes an ordinary .NET failure where a forward-declared base constructor could
silently be omitted. ConstructorChainingTests checks both declaration orders,
base-initialized fields, derived field initialization, and rejection without output
when a forward base has no matching overload. Existing constructor diagnostics remain
covered by ConstructorInitializerTests. This change adds no target mapping or runtime
contract configuration and does not depend on the experimental NeoCLR backend.

Validation (2026-10-04): the isolated `codex/fix-constructor-initializer-binding`
branch starts at main `210d891e0`. All 12 ConstructorInitializerTests and
ConstructorChainingTests pass on .NET 11, including separate source trees and
canonical symbol identity. The ordinary compiler sample returned 0 before the fix
because base initialization was omitted; the paired integration fixture returns 42
on .NET 10 and NeoCLR. This is not a claim of full test-suite coverage.

The isolated main-based correction is commit `2416a1646`; the integration branch
also passes its 22 existing EmissionCapabilityTests (34 focused tests total).

## Constructors across partial declarations

A partial type has one combined constructor set. An explicit instance constructor
or primary constructor in any part suppresses implicit default constructor
synthesis for the whole type. An empty part does not introduce another constructor
or an implicit zero-argument base call. A static constructor alone does not suppress
the ordinary instance default constructor. This shared declaration rule applies
before .NET or neoCLR constructor encoding; .NET remains the default target.

## Implicit value-type delegation

Structs and record structs retain a public implicit zero-argument constructor when
other constructors are declared. `init(value: int) : self()` resolves that symbol
and zero-initializes the receiver before the explicit constructor's field
initializers and body. A direct `Type()` call to the implicit constructor only
zero-initializes storage; it does not run those field initializers. This matches
C# value-type construction. An explicitly declared zero-argument constructor
continues to be an ordinary delegation target.

The implicit symbol has no physical CLI constructor method. General .NET emission
uses value initialization, and the shared portable constructor plan initializes
owned fields without a call to a nonexistent runtime method. neoCLR declaration
emission omits that semantic-only constructor as well. Validation covers .NET
runtime behavior and metadata, neoCLR semantic binding, and shared-plan admission;
this does not claim native execution of the new cases.

The existing portable record admission limit remains: modern .NET's imported
`IEquatable<T>` has an `allows ref struct` constraint that the bounded interface
contract does not yet admit. Portable-plan validation here covers ordinary structs;
record structs are validated through .NET execution and both semantic targets.
This is an implementation gap, not permanent neoCLR language exclusivity.
