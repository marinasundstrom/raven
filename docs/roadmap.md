# Raven MVP roadmap

Raven has enough language, compiler, tooling, and workload coverage to move from
feature exploration toward a coherent minimum viable product. The purpose of
the MVP is not to claim production readiness. It is to let people build and
evaluate representative .NET applications, understand Raven's design, and get
useful feedback when something goes wrong.

This roadmap is outcome-based rather than date-based. A phase is complete when
its exit criteria are met, not when a particular number of features have been
implemented.

## Product promise

Raven is a pragmatic, typed application language for .NET. It makes functional
composition, algebraic modeling, procedural code, and object-oriented design
complementary parts of one toolset while retaining direct access to the .NET
runtime and ecosystem.

The MVP should demonstrate four promises:

1. **Expressive application modeling.** Records, unions, patterns, functions,
   `Option`, and `Result` make domain states and transitions explicit.
2. **Practical .NET interoperability.** Raven applications can use established
   .NET libraries, frameworks, project infrastructure, and deployment paths.
3. **Useful compiler feedback.** Diagnostics and semantic tooling help users
   understand their programs without requiring compiler-internal knowledge.
4. **Representative workload coverage.** Raven works for recognizable
   application shapes, not only isolated language examples.

## Guiding constraints

- Prefer stabilization and consistency over expanding the syntax surface.
- Require a real workload or a demonstrated ergonomic gap before adding MVP
  language features.
- Fix compiler behavior rather than working around it in Raven application
  code.
- Keep user documentation aligned with implemented, tested behavior.
- Treat compiler crashes, silent miscompilations, and unreliable semantic
  answers as release blockers on supported paths.
- Be explicit about experimental areas and compatibility limits.
- Use Raven-authored applications and tools to expose genuine usability
  problems, without rewriting infrastructure merely for dogfooding.

## Phase 1: Demonstrable

Make Raven easy to encounter, understand, and show.

### Outcomes

- Define one supported installation path and one coherent first-run path from
  installation through `rvn new`, build, run, and test.
- Keep a five-minute introduction that communicates Raven's core design rather
  than presenting a feature inventory.
- Select three to five hero workloads from the existing project samples.
- Give every hero workload a clear thesis, short README, expected output, and
  reproducible commands.
- Explain the intended user and the Raven advantage for each workload.
- Publish a concise support statement and list of known limitations.

### Candidate hero workloads

The final set should be small. These existing samples are candidates, not an
obligation to promote every one:

| Workload | Candidate sample | What it should prove |
| --- | --- | --- |
| First application or CLI | `samples/projects/hello-world` and `samples/scripts` | Installation, project creation, arguments, build, and run |
| Domain workflow | `samples/projects/fulfillment-workflow` | Records, unions, patterns, `Option`, `Result`, and explicit state transitions |
| Web service | `samples/projects/aspnet-minimal-api` or `samples/projects/efcore-vehicle-costs` | ASP.NET Core, dependency injection, JSON, persistence, and ordinary .NET interop |
| Data or device workload | `samples/projects/greenhouse-monitor` | Async streams, cancellation, telemetry modeling, and deployment or Native AOT |
| Mixed ecosystem application | An existing mixed Raven/C# sample, or a focused new one | Gradual adoption and bidirectional Raven/C# boundaries |

Specialized demonstrations such as macros, analyzers, Blazor integration, and
nanoFramework remain valuable, but should not obscure the initial application
story. They can become secondary showcases after the core hero set is clear.

### Exit criteria

- A new evaluator can follow the documented path from a fresh supported
  environment without repo-specific knowledge.
- All selected hero workloads build and run through their documented commands.
- The landing page points to the first-run experience, hero workloads, and
  limitations.
- Each advertised language construct and tooling flow is implemented and
  covered by a relevant test or continuously verified sample.

## Phase 2: Dependable

Make Raven safe and predictable to explore for non-trivial applications.

### Outcomes

- Eliminate compiler crashes and silent miscompilations along the supported
  first-run and hero-workload paths.
- Stabilize the syntax and semantics exercised by those workloads.
- Improve parser and binder recovery so common mistakes produce actionable
  diagnostics.
- Keep public semantic APIs correct and responsive under incremental edits.
- Make diagnostics, completion, hover, go-to-definition, and document updates
  dependable for the supported language subset.
- Exercise restore, build, run, test, clean, and package-reference workflows.
- Establish regression coverage at diagnostic, symbol, operation, metadata,
  and observable runtime boundaries.
- Define the compatibility expectations for MVP releases.

### Exit criteria

- Hero workloads run in CI and failures identify the affected supported
  scenario.
- Invalid variations of the hero workloads report useful diagnostics rather
  than crashing or hanging.
- The documented editor operations behave consistently on those workloads.
- Repeated edits and builds do not depend on stale semantic or incremental
  state.
- Known release-blocking defects are tracked by supported scenario, with a
  focused regression test for every completed compiler fix.

## Phase 3: Evaluatable MVP release

Package the demonstrable and dependable work into a release that outsiders can
assess honestly.

### Outcomes

- Distribute the `rvn` frontend, `rvnc` compiler driver, build assets, language
  server, and `Raven.Core` through a documented versioned mechanism.
- Provide a release notes page that distinguishes stable MVP behavior,
  experimental behavior, known limitations, and breaking-change risk.
- Publish the hero workloads with tested version requirements.
- Provide a short Raven-versus-C# comparison based on the same representative
  problem, including the tradeoffs rather than only Raven's advantages.
- Collect structured feedback about installation, diagnostics, interop,
  language ergonomics, editor behavior, and missing application capabilities.

### Release readiness gate

The MVP is ready to publish when all of the following are true:

- A supported installation does not require knowledge of the Raven repository
  layout.
- `rvn new`, restore, build, run, and test form a coherent documented workflow.
- Hero workloads build and run from clean checkouts in CI.
- Supported paths do not contain known compiler crashes or silent
  miscompilations.
- Common user mistakes receive actionable diagnostics.
- Core editor operations are usable on the hero workloads.
- The language semantics used by the hero workloads are documented and tested.
- Experimental features and compatibility expectations are clearly labeled.
- The release has a reproducible smoke-test checklist.

A suitable release statement is:

> Raven MVP is ready for building representative .NET applications and
> evaluating the language's design. The compiler and tooling remain
> experimental, and compatibility is not yet guaranteed.

## Phase 4: Post-MVP stabilization

Use external evaluation and continued dogfooding to decide what deserves to
become durable.

### Outcomes

- Prioritize issues by their effect on supported workloads and user trust.
- Refine language consistency, diagnostics, compiler performance, and editor
  responsiveness before reopening broad syntax exploration.
- Define compatibility, deprecation, and language-change processes.
- Clarify extension points for compiler services, analyzers, formatters,
  macros, and build integration.
- Separate stable, preview, and research surfaces in documentation and tests.
- Evolve the Playground from a single source buffer to small multi-file
  projects with a tabbed editor, while keeping shared links deterministic and
  safe to load and extending the existing compiler-worker protocol to carry a
  versioned project snapshot.
- Expand the showcase only when a workload demonstrates a distinct, repeatable
  use case.

## Dogfooding questions

Raven-authored workloads should continuously test the language as an
application-development experience:

- Which common operations are unexpectedly verbose?
- Where does inference make behavior difficult to predict?
- Which diagnostics explain a symptom rather than the actual mistake?
- Where does .NET interop expose unnecessary compiler or runtime details?
- Which constructs work in isolated tests but become awkward at application
  scale?
- Do refactoring and incremental editing remain reliable as projects grow?

Answers should normally become focused compiler fixes, documentation changes,
or regression tests. They should become new language features only when the
existing model cannot express the needed behavior coherently.

## Prioritization rule

Until the evaluatable MVP release, proposed work should answer at least one of
these questions:

1. Does it complete or clarify a supported first-run or hero-workload path?
2. Does it fix behavior that undermines correctness, diagnostics, tooling, or
   user trust on those paths?
3. Does it provide evidence needed to decide whether an existing language or
   compiler design is viable?

Work that answers none of them belongs in the post-MVP backlog unless it
addresses a critical repository-wide concern.
