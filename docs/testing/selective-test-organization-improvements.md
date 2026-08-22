# Selective Test Organization Improvements

## Purpose

Make the smallest trustworthy test run easy to select from a change set. The
organization should answer two different questions without requiring a broad
baseline:

1. Which language or tooling behavior can this change affect?
2. At which compiler or product boundary does that behavior need validation?

The existing [test impact map](test-impact-map.md) remains the practical guide
while this proposal is implemented. This document outlines how to make that
selection more precise, maintainable, and eventually automatable.

## Problems to solve

- Feature suite membership is encoded as long fully-qualified-name filters in a
  shell script. Renaming or moving a test can silently change coverage.
- Folder, class-name, and manually maintained exclusion rules are all used to
  infer whether a test is fast, runtime-heavy, sample-backed, or process-heavy.
- A feature suite often mixes syntax, binding, lowering, emission, runtime, and
  editor coverage. This makes it difficult to run only the layer affected by a
  change.
- Some unit projects contain tests that load repository samples or consume
  outputs produced by another build. They can pass in a developed workspace but
  fail in a clean checkout.
- Shared compiler changes have no small, named layer contract suite between a
  focused feature filter and the full baseline.
- The current impact map selects by broad area but cannot compose feature and
  layer ownership, such as “macro parsing only” or “pattern binding plus
  observable runtime behavior.”
- Test duration, dependencies, and historical failure relationships are not
  recorded in a form that can improve future selection.

## Proposed classification model

Classify each test on three independent axes. Do not encode one axis indirectly
through another.

| Axis | Examples | Purpose |
|---|---|---|
| Feature | `macros`, `patterns`, `unions`, `functions`, `imports`, `nullability` | Select behavior owned by the change |
| Layer | `syntax`, `binding`, `semantic-api`, `operations`, `lowering`, `emit`, `runtime`, `lsp-presentation`, `workspace`, `project-system` | Select the affected implementation boundary |
| Profile | `hermetic`, `fixture`, `process`, `runtime`, `sample`, `performance` | Select the required environment and cost |

A test can belong to multiple features, but should normally have one primary
layer and one profile. Cross-feature tests are useful when they protect a real
contract; they should not become a substitute for focused tests at the owning
layer.

Use explicit test metadata rather than parsing class names. With xUnit this can
begin with traits, for example:

```csharp
[Trait("Feature", "macros")]
[Trait("Layer", "binding")]
[Trait("Profile", "hermetic")]
public sealed class FreestandingMacroSemanticTests
```

Common base classes or custom attributes can reduce repetition after the
taxonomy has stabilized. The initial implementation should favor transparent
xUnit traits that work with `dotnet test --filter`.

## Test boundary rules

### Syntax

- Parse source directly and assert syntax kinds, spans, trivia, and recovery.
- Do not create compilations unless the parser contract specifically depends on
  contextual information.
- Keep lexer, parser, syntax factory, and syntax-tree contract tests selectable
  independently.

### Binding and semantic APIs

- Prefer in-memory syntax trees, compilations, references, and macro definitions.
- Binding tests should assert symbols, types, conversions, diagnostics, or
  operations without emitting an assembly.
- Test public semantic APIs separately from binder internals when caching,
  incremental state, or language-service behavior is part of the contract.
- Shared binder changes should have a compact `binding-contracts` suite covering
  declaration binding, lookup, overload resolution, conversions, and diagnostic
  caching across representative features.

### Lowering

- Test lowered behavior at the nearest stable representation only when that
  representation is itself the contract.
- Prefer observable behavior tests over instruction-sequence assertions.
- Keep temporary lowered-shape development tests out of normal selection.

### Emit and runtime

- Separate metadata-shape tests from executable runtime tests. They have
  different failure modes and costs.
- Run focused runtime overlap for the changed feature before the broad isolated
  runtime suite.
- Tests requiring a child process, reflection load context, native toolchain, or
  emitted application should declare that profile explicitly.

### Language server

- `lsp-presentation` tests should use in-memory compiler inputs and test request
  mapping or presentation logic without loading repository samples.
- `workspace` tests should own document lifecycle, incremental compilation,
  project association, and concurrency behavior.
- Project-backed, MSBuild, external analyzer, and multi-process behavior belongs
  in language-server integration tests, not the unit project.
- For semantic failures, add or run the compiler-layer test before the LSP test.
  The LSP test should prove only the editor-facing contract that the compiler
  test cannot cover.

### Fixtures, samples, and clean environments

- A `hermetic` test must not depend on repository `bin`/`obj` outputs, network
  access, an installed SDK package, shared mutable files, or another test's
  execution order.
- Inline source and in-memory references are preferred for unit and presentation
  tests.
- When a real project layout is essential, copy a minimal fixture into a unique
  temporary directory and build every required dependency explicitly.
- Repository samples validate product scenarios. They should not also serve as
  incidental unit-test fixtures.
- Tests asserting an empty diagnostic set are appropriate only when the test
  owns the complete compilation environment. Otherwise assert the diagnostic
  contract relevant to the behavior under test.
- CI should periodically run all `Profile=hermetic` tests from a fresh checkout
  with no prebuilt sample outputs.

## Suite structure

Compose suites from the classification axes instead of maintaining unrelated
lists of test names.

### Feature-layer suites

Examples:

- `macros + syntax + hermetic`
- `macros + binding + hermetic`
- `macros + lsp-presentation + hermetic`
- `patterns + binding + hermetic`
- `patterns + runtime`
- `imports + project-system + fixture`

The common inner-loop command should select a feature and one or more layers.
Requesting only a feature should default to its hermetic syntax, binding, and
semantic-API coverage. Runtime, process, sample, and performance profiles must
remain explicit.

### Layer contract suites

Add small, stable suites for changes to shared infrastructure:

- `syntax-contracts`
- `binding-contracts`
- `semantic-api-contracts`
- `lowering-contracts`
- `emit-contracts`
- `workspace-contracts`

Each contract suite should use representative cases from several features and
remain intentionally bounded. These suites fill the gap between one feature
filter and the full baseline.

### Cross-layer feature journeys

Keep a small number of tests that prove a feature across necessary boundaries,
for example parse → bind → emit → execute or compiler → workspace → LSP. Mark
these as integration or runtime profiles and run them only when a change can
affect that boundary. Do not make every feature test a full journey.

## Change-to-test selection

Introduce a machine-readable manifest, for example
`eng/testing/test-impact.yml`, as the source of truth for source ownership and
suite composition. It should map source paths to primary features, layers, and
required fan-out rules.

Illustrative shape:

```yaml
paths:
  src/Raven.CodeAnalysis/Syntax/**:
    layers: [syntax]
    contracts: [syntax-contracts]
  src/Raven.CodeAnalysis/Binding/Macros/**:
    features: [macros]
    layers: [binding, semantic-api]
  src/Raven.LanguageServer/SemanticTokensHandler.cs:
    layers: [lsp-presentation]
    features: [semantic-classification]

fanout:
  syntax-model-inputs:
    build: codex-build
    contracts: [syntax-contracts, binding-contracts]
  lowering:
    add_profiles_when_changed: [emit]
```

The selector should:

1. Include tests changed directly by the change set.
2. Map changed production paths to features and layers.
3. Add only declared downstream contracts.
4. Intersect the result with the cheapest valid profile by default.
5. Explain every selected suite and every escalation.
6. Refuse to claim a trustworthy selection when a changed path has no mapping;
   in that case it should recommend the full baseline and identify the missing
   manifest entry.

Avoid an unrestricted dependency-transitive algorithm. Compiler dependencies
fan out too broadly for that to remain selective. Curated boundary rules are
more useful, and historical evidence can refine them over time.

## Proposed commands

Evolve `test-feature-suite.sh` into, or wrap it with, a selector that supports:

```bash
# Show what would run and why.
scripts/test-select.sh --changed origin/main --explain

# Explicit inner-loop selection.
scripts/test-select.sh --feature macros --layer syntax,binding

# Add expensive overlap deliberately.
scripts/test-select.sh --feature macros --profile runtime

# Run a shared layer contract.
scripts/test-select.sh --contract semantic-api-contracts

# Audit taxonomy and unmapped paths without running tests.
scripts/test-select.sh --validate-manifest
```

The first version can emit normal `dotnet test --filter` commands. Selection
logic and test execution should remain separate so CI, developers, and release
automation can inspect the same plan.

## Folder and project organization

Use folders to make ownership understandable, but use metadata for selection.
Move gradually toward the following compiler-test layout:

```text
Syntax/<Feature>/
Semantics/<Feature>/
SemanticApi/<Feature>/
Operations/<Feature>/
Lowering/<Feature>/
Emit/<Feature>/
Runtime/<Feature>/
ProjectSystem/
```

For language-server tests, separate presentation handlers, workspace lifecycle,
and project-backed integration. Do not split test projects solely for visual
organization. A new project is justified when it provides useful process,
dependency, framework, or fixture isolation that traits cannot provide.

## Validation tiers

| Tier | Contents | Typical use |
|---|---|---|
| T0 | Changed tests and direct hermetic unit tests | Every edit |
| T1 | Feature-layer suites and one relevant contract suite | Before committing a scoped change |
| T2 | Subsystem integration, focused emit/runtime, workspace fixtures | When the changed boundary requires it |
| T3 | Bounded main CI contracts | Every pushed commit |
| T4 | Full baseline, isolated runtime, samples, distribution, performance | Scheduled, stabilization, or release gates |

The full baseline remains valuable as a broad confidence and selector-audit
tool. It should not be the normal response to uncertainty that can instead be
resolved by adding a missing feature, layer, profile, or path mapping.

## CI and feedback loop

- Pull requests should publish the selected suites and the reasons for each
  selection.
- Main CI should keep a bounded, stable contract gate independent of the
  selector so a bad mapping cannot remove all safety coverage.
- Nightly or scheduled runs should execute the full baseline, runtime,
  project-system, sample, and performance partitions as appropriate.
- Compare failures from broad runs with the suites selected for the originating
  change. A missed regression should normally add a mapping or boundary contract,
  not permanently broaden every run.
- Record per-test duration, failure history, profile, and clean-environment
  status. Use duration to batch tests, not to infer semantic ownership.
- Add a manifest audit that detects unknown feature/layer/profile values, test
  classes without metadata, references to missing tests, and source areas with
  no mapping.

## Process contracts and documentation synchronization

Selective testing must be part of Raven's normal engineering processes. A
selector that is documented only here will drift from agent guidance, release
instructions, scripts, and GitHub Actions.

Use the following ownership model to avoid copying the same rules into several
documents:

| Concern | Authoritative source | Other consumers |
|---|---|---|
| Test taxonomy, boundaries, and migration | This document | `AGENTS.md`, Raven test skills, contributor documentation |
| Source-to-test mappings and suite membership | `eng/testing/test-impact.yml` | Selector, manifest audit, CI job matrix |
| Immediate human selection guidance | `docs/testing/test-impact-map.md` | `AGENTS.md`, debugging and feature workflows |
| Compiler debugging sequence | `.agents/skills/raven-debug-compiler/SKILL.md` | Bug-fix and triage work |
| LSP debugging sequence | `.agents/skills/raven-lsp-debug/SKILL.md` | Language-server investigations |
| Test stabilization and cleanup | `.agents/skills/raven-test-triage/SKILL.md` and `raven-test-cleanup/SKILL.md` | Baseline and coverage work |
| Release contract and publication order | `docs/compiler/distribution.md` | Release scripts and distribution workflows |
| Executable enforcement | `scripts/test-*.sh`, release validation scripts, GitHub workflows | Documentation should describe rather than duplicate implementation details |

Documents and skills should link to their authoritative source and state only
the extra decisions owned by that process. A change to taxonomy, suite
semantics, escalation rules, or release gates is incomplete until the affected
consumers and executable checks are updated in the same commit.

### Development and feature workflow

Update `AGENTS.md` and `raven-feature-workflow` to use a common sequence:

1. Ask the selector for the smallest pre-change plan and review its explanation.
2. Establish that baseline once.
3. Add or update tests at the closest owning layer.
4. Run changed tests, then the feature-layer suite, then only declared boundary
   overlap.
5. Record a missing mapping when the selector cannot explain the affected area.
6. Escalate to a broad baseline only for an explicit trigger, not as a default
   confidence ritual.

Feature changes that span syntax, binding, and editor support should still run
those layers separately first. A combined journey test is the final boundary
check, not the primary diagnostic tool.

### Debugging workflow

Update `raven-debug-compiler`, `raven-lsp-debug`, and `raven-test-triage` so a
debugging session preserves the failure boundary:

1. Reproduce the exact failing test or smallest source sample.
2. Classify the failure by feature, layer, and profile.
3. Cross-check the nearest lower layer before debugging a higher layer. For
   example, check semantic classification before semantic-token presentation,
   and binding before emission.
4. Determine whether the failure is hermetic. Reproduce in a clean worktree when
   generated files, sample outputs, project references, installed packages, or
   test order could influence it.
5. Fix product behavior or fixture ownership at the layer where it belongs.
6. Run the focused regression, its feature-layer suite, and any affected
   boundary contract.
7. Add a selector mapping or hermeticity rule when the regression exposed a gap
   in test organization.

Debugging guidance should explicitly distinguish these outcomes:

- a product regression;
- a stale expectation;
- a non-hermetic fixture;
- a test-order or concurrency dependency;
- a missing change-to-test mapping;
- a failure in release or CI infrastructure.

That distinction determines which test profiles need rerunning. A fixture-only
repair should not automatically trigger every compiler baseline, while a shared
binder repair should run the binding contract suite even if the original
failure appeared only in the LSP.

### Release workflow

Update `docs/compiler/distribution.md`, `prepare-release.sh`, and release guidance
to separate four release phases:

1. **Candidate selection:** choose one commit and generate an explained test plan
   from all changes since the previous release.
2. **Stabilization:** run targeted suites first. Commit each product, test, or
   fixture repair separately and rerun the affected selection after each repair.
3. **Candidate qualification:** once the candidate stops changing, run the broad
   release gates once on the exact commit: bounded CI, full baseline, isolated
   runtime, package validation, and the required platform or sample checks.
4. **Publication verification:** tag only the qualified commit, build all
   artifacts from that tag, publish, then test public installation and package
   propagation.

If qualification finds a failure, the candidate becomes unqualified. Diagnose
and repair it with focused tests before deciding which broad gates must be
repeated. The release record should show why a broad gate was repeated or why a
test-only/hermetic-fixture commit required only a bounded subset plus exact-SHA
CI. Publication must never rely on successful checks from an earlier commit.

Create a machine-readable release evidence artifact containing at least:

- release version and candidate commit SHA;
- base release tag or comparison commit;
- selector plan and explanation;
- test commands, profiles, results, and timestamps;
- clean-worktree status for hermetic and package checks;
- package version/provenance validation;
- required GitHub workflow run identifiers;
- tag and published artifact checksums after publication.

`validate-release.sh --require-clean` should validate the candidate metadata and
evidence schema locally. The distribution workflow should independently verify
the tag, exact SHA, required main-CI conclusion, package versions, and checksums.

### Workflow changes

Workflow changes should be introduced in stages so selection bugs cannot reduce
coverage silently:

1. Add a manifest-audit job to `.github/workflows/ci.yml`.
2. Add a report-only selection job that publishes the plan for each change.
3. Convert the plan into a job matrix while retaining the current bounded main
   contract gate.
4. Add scheduled full baseline, runtime, project-system, sample, and hermetic
   clean-checkout jobs as selector backstops.
5. Make release qualification produce and upload the release evidence artifact.
6. Require `distribution.yml` to consume evidence for the exact tagged commit.
7. Keep `installation.yml` as a post-publication test of public artifacts rather
   than treating successful packaging inside the repository as installation
   proof.

Path filters in GitHub Actions should decide whether an entire product area is
irrelevant, not encode detailed compiler impact logic. The manifest and
selector should own fine-grained selection so local runs and CI produce the same
answer.

### Change checklist for process updates

When changing test or release process behavior, review this list in the same
change:

- `AGENTS.md`;
- `docs/testing/test-impact-map.md` and this strategy;
- applicable Raven skills under `.agents/skills/`;
- `scripts/test-feature-suite.sh`, `test-baseline.sh`, `test-runtime-isolated.sh`,
  and the future selector/manifest;
- `docs/compiler/distribution.md`, `prepare-release.sh`,
  `validate-release.sh`, and packaging scripts when release behavior changes;
- `.github/workflows/ci.yml`, `distribution.yml`, `installation.yml`, and
  specialized validation workflows when their contract changes.

Add an automated documentation-reference audit where practical. It should catch
renamed scripts, removed suite names, obsolete workflow inputs, and release
commands whose documented form no longer matches executable usage.

## Migration plan

### Phase 1: make the taxonomy explicit

- Agree on the initial feature, layer, and profile vocabulary.
- Add traits to the current named feature suites and all known heavy tests.
- Mark repository-sample and child-process dependencies explicitly.
- Add hermeticity guidance to test utilities and contribution documentation.

### Phase 2: replace brittle filters

- Generate current feature-suite filters from traits.
- Add the layer contract suites.
- Remove class-name scraping and hand-maintained heavy-test name lists once
  equivalent metadata coverage is validated.

### Phase 3: add change-based selection

- Create the source-path impact manifest.
- Implement `test-select.sh --changed --explain` in report-only mode.
- Compare its recommendations with actual full-baseline failures before using
  it as a CI gate.

### Phase 4: reorganize and enforce boundaries

- Move tests into clearer feature/layer folders as files are touched.
- Convert unit tests that load samples into in-memory tests or explicit fixtures.
- Add clean-checkout hermetic-suite validation.
- Split projects only where stronger runtime or dependency isolation is needed.

### Phase 5: synchronize engineering and release processes

- Update `AGENTS.md` and Raven skills to consume selector explanations and the
  shared escalation rules.
- Update the distribution guide and release scripts around candidate,
  stabilization, qualification, and publication phases.
- Add release evidence generation and exact-commit verification.
- Move CI from report-only selection to a guarded matrix backed by scheduled
  broad runs.
- Add drift checks across manifest values, documented commands, script options,
  and workflow inputs.

## Success criteria

- A scoped source change produces an explainable test plan without manual class
  name lookup.
- Syntax-only and binding-only changes can be validated independently for a
  feature.
- Unit and LSP presentation suites pass in a clean checkout without prebuilt
  samples.
- Runtime, process, sample, and performance tests are never pulled into the fast
  path accidentally.
- Unmapped source changes are visible rather than silently under-tested.
- Median inner-loop validation time decreases while scheduled broad runs do not
  reveal an increasing rate of regressions missed by the selected suites.

## Initial implementation backlog

1. Inventory existing tests into feature, layer, and profile counts.
2. Define the canonical vocabulary and naming rules.
3. Trait the current `macros`, `patterns`, `unions`, and
   `overload-resolution` suites as pilots.
4. Trait every test currently found through heavy-name and CodeGen-folder
   exclusions.
5. Create `syntax-contracts`, `binding-contracts`, and
   `workspace-contracts` as the first shared-layer suites.
6. Add a clean-checkout hermetic run for compiler and LSP unit tests.
7. Build the manifest validator and explanation-only selector.
8. Measure selector precision against several weeks of broad-run results before
   changing required CI behavior.
9. Update development, debugging, triage, feature, and release guidance to the
   shared process contracts.
10. Add report-only CI selection and a release-evidence prototype before making
    either one a required gate.
