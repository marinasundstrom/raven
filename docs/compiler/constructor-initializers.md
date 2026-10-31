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
