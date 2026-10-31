# ADR-0004: Port first, maintain both compilers, then freeze C#

- Status: Accepted
- Date: 2026-10-31
- Owners: Raven project maintainers
- Supersedes: the stage ordering and C# freeze timing in [ADR-0002](0002-qualify-bootstrap-foundation-after-stabilization.md)

## Context

The earlier plan required a frozen C# foundation and a C# implementation adopting
Raven.Core API types before source porting. The author instead directs a Raven
source port alongside the original C# codebase, using real porting work to expose
compiler bugs and language/API gaps. The destination remains an idiomatic Raven
compiler, eventually running natively on neoCLR.

## Decision

1. Maintain the original C# and Raven codebases during the port. Start with
   faithful, recognizable translations rather than broad idiomatic redesign.
2. Fix shared bugs and gaps in both implementations, with regressions; record
   corrections for components not yet ported. Translation-only bugs need no
   artificial C# counterpart.
3. Reuse existing tests wherever possible. Adapt or port tests that cannot run
   against the Raven implementation, preserving their assertions and coverage.
4. Establish behavioral parity, reproducible builds and reliable self-hosting
   before freezing C# development. Self-compilation alone does not qualify parity.
5. Retain frozen C# source/artifacts as the trusted seed and reference. Continue
   normal development in Raven, progressively adopting idiomatic patterns.
6. Later move the compiler host to neoCLR by implementing the required APIs and
   host services there. Keep compiler host and output target independent; .NET
   remains the primary/default output target throughout.

Adopting Raven-native result types inside C# is optional, not a mandatory stage
before the source port. Immutable seed artifacts may be recorded during active
C# maintenance; this does not freeze the development line. Existing tags and seed
tooling are not renamed or reinterpreted by this decision.

## Consequences

- Stabilization and porting proceed together. Differential tests and a defect
  ledger distinguish reference-compiler bugs, translation defects and API gaps.
- Dual maintenance costs are deliberate until parity and the C# freeze.
- Idiomatic API and architecture changes are incremental, with larger redesigns
  following parity rather than obscuring the initial translation.
- ADR-0002's provenance, regression and compatibility requirements remain useful;
  its mandatory API-first stages and early freeze/backport policy are superseded.
- Native host API availability is a later qualification task, not a prerequisite
  for starting the .NET-hosted source port.

The [bootstrap procedure](../bootstrap-procedure.md) defines execution, test reuse,
defect handling, freeze acceptance and artifact identity. No compiler port,
bootstrap freeze, release tag or native-host qualification is claimed by this ADR.
