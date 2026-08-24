# ADR-0001: Preserve preview.14 as the pre-bootstrap foundation

- Status: Superseded by [ADR-0002](0002-qualify-bootstrap-foundation-after-stabilization.md)
- Date: 2026-08-23
- Owners: Raven project maintainers

## Context

Raven is approaching the point where its compiler APIs should use Raven-native
domain shapes such as `Option<T>`, `Result<T, TError>`, named unions, and ad hoc
unions. Those types are authored in `Raven.Core`. Standard macro providers are
in the version-matched `Raven.Macros` library and increasingly exercise the
same public compiler APIs that external tools use.

The compiler implementation is still predominantly C#. Porting that codebase to
Raven immediately would combine three independent risks: changing public API
contracts, changing the compiler implementation language, and closing the
bootstrap dependency cycle. A failure would be difficult to attribute to one
of those changes.

Release `v0.1.0-preview.14` contains the stabilized macro carrier model,
Raven-authored standard macro implementations, typed macro expression
contracts, matching SDK and editor artifacts, and public distribution and
installation verification. It provides a reproducible compiler capable of
building the next stage.

## Decision

Treat `v0.1.0-preview.14` as the immutable stage-0, pre-bootstrap foundation.
Preserve its tag and complete lockstep artifact family so later stages can be
built, diagnosed, and rolled back with a known compiler.

The next compiler line may adopt Raven-native compiler API contracts before any
compiler component is ported to Raven:

1. Keep the current C# compiler as the implementation and behavioral oracle.
2. Establish an explicit staged build in which exact stage-0 artifacts supply
   bootstrap inputs. A compiler being built must not implicitly rebuild or
   discover its own foundational inputs.
3. Prove `Raven.Core` assembly identity, loading, packaging, and provenance
   before exposing its types from compiler APIs.
4. Migrate coherent API families incrementally to `Option`, `Result`, or unions
   according to the semantic meaning of each outcome.
5. Exercise every migrated contract from both Raven and C#, including standard
   macros, language services, analyzers, packaging, and clean public consumers.
6. Keep `Raven.Macros` above `Raven.CodeAnalysis` in the dependency graph. The
   compiler hosts macro contracts, but does not depend on the standard macro
   provider implementation library.
7. Port compiler components to Raven only in a later phase, behind stable
   boundaries and differential tests against the C# implementation.
8. Preserve the complete compiler baseline and the standalone and project
   sample corpora as stage-transition gates. A bootstrap stage is not accepted
   merely because it can build itself; it must continue to compile and, where
   applicable, run the same representative Raven programs as stage 0.

Defects discovered while porting must be classified before they are fixed:

- A stage-0 compiler or runtime defect is reduced, covered, and fixed on the
  maintained pre-bootstrap line first, then carried into the port.
- An unclear language rule or public contract is decided and documented for
  both lines before either implementation becomes the oracle.
- A defect confined to the Raven port is fixed forward and guarded by
  differential coverage; it is not backported into unrelated stage-0 code.
- A bootstrap-only accommodation is kept at an explicit stage boundary and is
  not presented as ordinary language behavior.

This classification determines whether a fix is backported. Backports protect
the trusted foundation; they must not turn the foundation branch into a mirror
of every structural change made during the port.

The detailed result-shape and migration guidance lives in
[Raven-native Compiler API result shapes and pre-bootstrap
adoption](../../api/result-shapes.md).

## Consequences

- Raven-facing APIs can become idiomatic before the compiler source is ported.
- C# remains a valid implementation and interoperability language; it may
  construct Raven-authored CLR types more explicitly than Raven callers do.
- The project accepts intentional compiler API breaks while the API remains
  experimental, rather than preserving nullable transitional signatures.
- Builds need explicit stage and artifact provenance. Repository-local package
  feeds, global SDKs, stale outputs, and accidental assembly-copy precedence
  cannot be accepted as bootstrap inputs.
- Preview.14 must remain downloadable and its release tag must not move.
- Internal compiler cleanup is not implied by a public contract migration.
  Broad internal Raven-style refactoring remains separate from both API
  adoption and source porting.
- Optional macro inputs should move toward `Option<SyntaxType>` rather than
  nullable default parameters that obscure meaning or violate Raven parameter
  ordering expectations.
- Release and bootstrap validation takes longer because it includes the full
  baseline plus build-and-run coverage for the maintained sample corpora. That
  cost is intentional: the samples exercise integration paths and Raven-shaped
  workloads that unit tests alone do not cover.

## Alternatives considered

### Port the compiler to Raven before changing its APIs

Rejected because it combines contract design, source translation, and
bootstrap closure in one step. It would make the C# oracle less useful and make
regressions harder to classify.

### Keep nullable C#-shaped APIs until self-hosting is complete

Rejected because implementation language does not require a C#-shaped public
model. Raven-authored `Option`, `Result`, and union types are ordinary CLR types
and can be returned by the existing C# implementation once the build boundary
is sound.

### Build foundational inputs from the current source tree in one graph

Rejected because it creates or hides a bootstrap cycle. The stage producing an
input must be explicit and independently reproducible.

### Let `Raven.CodeAnalysis` depend on `Raven.Macros`

Rejected because the standard provider library consumes compiler contracts and
is replaceable/versioned as a higher layer. Reversing that edge would couple the
compiler API to one macro distribution and create another cycle.

## Follow-up

- Design and test the deterministic stage-0/stage-1 build and assembly identity
  model.
- Inventory public nullable, boolean-plus-out-value, exception, and weakly typed
  compiler API contracts by semantic meaning.
- Select a small pilot API family, with optional macro syntax inputs as one
  candidate.
- Add Raven and C# contract tests before changing each family.
- Record a reproducible baseline and sample-corpus result for the stage-0 tag,
  including target-framework and toolchain provenance.
- Establish a small porting ledger that records each discovered issue, its
  classification, the chosen backport target, and the regression coverage.
- Record subsequent architectural choices as new ADRs and supersede this record
  if the foundation or staging strategy changes.
