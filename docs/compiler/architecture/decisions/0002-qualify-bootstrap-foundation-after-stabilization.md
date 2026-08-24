# ADR-0002: Qualify the bootstrap foundation after stabilization

- Status: Accepted
- Date: 2026-08-24
- Owners: Raven project maintainers
- Supersedes: [ADR-0001](0001-pre-bootstrap-foundation.md)

## Context

ADR-0001 designated `v0.1.0-preview.14` as Raven's immutable bootstrap-v1
foundation. Continued macro, Core, SDK, editor, target-framework, and compiler
stabilization has identified additional integration paths and known skipped
runtime cases that should be addressed before Raven deliberately depends on a
foundation release for its next compiler stage.

The bootstrap plan remains staged: first establish a trusted compiler and
Raven-authored foundation, then adopt Raven-native compiler API contracts while
the implementation remains C#, and only later port compiler components to
Raven. The point being revised is when the foundation is selected.

## Decision

Select and freeze the bootstrap-v1 foundation only after the current
pre-bootstrap stabilization pass satisfies the documented compatibility gates.
The exact version and commit remain a release-candidate result, not an input
assumption.

The qualifying release must:

1. use the full C# compiler to compile and validate the first trusted
   Raven-authored `Raven.Core` as the foundational bootstrap artifact;
2. validate the version-matched `Raven.Macros` as a higher-layer dogfooding and
   compatibility artifact without adding it to the compiler dependency closure;
3. compile and run representative compiler-shaped Raven programs derived from
   recurring patterns in the C# compiler;
4. pass semantic, runtime, supported-target, sample, packaging, and public
   consumer gates with explicit toolchain provenance;
5. resolve or explicitly classify known skipped failures within the intended
   compiler-writing subset;
6. preserve each stabilization fix as focused regression coverage that can be
   carried or backported independently; and
7. check the first trusted target-specific Core assemblies and their manifest
   into the repository, and retain the immutable C# compiler release, checksums,
   and source tag that can reproduce them. Release-matched Macros/SDK/editor
   artifacts remain product validation evidence but are not required inputs to
   bootstrap v2; and
8. apply an immutable annotated bootstrap tag to the exact qualified commit for
   v1, v2, and v3. Bootstrap tags complement public SemVer release tags and are
   never moved; a corrected foundation uses a new revision tag.

After that freeze:

- bootstrap v2 may adopt foundation-built `Option`, `Result`, and
  union contracts while remaining implemented in C#;
- normal bootstrap-v2 builds consume the hash-verified v1 Core binaries checked
  into the repository and do not rerun the v1 compiler; and
- compiler source porting begins only after those native API boundaries and the
  compiler-writing subset are stable; completion of that port is bootstrap v3.

The detailed stages, dogfooding ladder, findings, and defect ledger are defined
in the [staged bootstrap procedure](../bootstrap-procedure.md).

## Consequences

- Preview.14 remains a historical published release but is no longer assumed
  to be the final bootstrap foundation.
- Foundation selection is evidence-based and may delay the stage boundary
  while defects in compiler-writing patterns, Core, Macros, or packaging are
  still unresolved.
- The chosen foundation version cannot be named until qualification finishes;
  release records must bind it to an exact commit and artifact checksums.
- More Raven-authored infrastructure is exercised before the source port,
  reducing the chance that porting discovers basic language or runtime defects
  that are difficult to backport.
- The C# compiler remains the oracle through bootstrap v2 and during
  incremental work toward bootstrap v3.
- Backporting after the v1 freeze is exceptional. A later improvement stays on
  the v2 or v3 line unless the frozen compiler/Core boundary demonstrably
  prevents rebuilding Core or supplying a required next-version dependency.
- A dedicated exceptional script can reproduce candidate v1 Core binaries from
  the frozen compiler/tag, but it cannot overwrite the checked-in seed. Any
  seed update requires a new immutable foundation revision and explicit review.
- Compiler product version, Core package/file version, Core CLR assembly
  compatibility version, bootstrap tag, source commit, and binary hashes are
  recorded independently even when public packages initially ship in lockstep.

## Alternatives considered

### Keep preview.14 as the final foundation regardless of later findings

Rejected because known integration and runtime gaps discovered after that
release would immediately become inherited bootstrap debt.

### Start API migration from an unfrozen repository build

Rejected because it obscures Core assembly provenance and permits accidental
cycles or stale artifact selection.

### Start porting the compiler while stabilization continues

Rejected because a mismatch could not be attributed reliably to the C# oracle,
the Raven implementation, or an unsettled language rule.

## Follow-up

- Complete the compiler-source-derived workload inventory and regression
  probes.
- Resolve or classify skipped runtime cases in the compiler-writing subset.
- Expand Core and macro metadata/behavior coverage where the foundation gates
  reveal gaps.
- Run the complete release and bootstrap compatibility gates on a clean
  candidate commit.
- Record the selected version, commit, SDK, targets, checksums, and test results
  at freeze time.
- Begin bootstrap-v2 API adoption only from those exact artifacts.
