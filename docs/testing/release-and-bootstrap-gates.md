# Release and bootstrap compatibility gates

The stable `0.1.0` release candidate that becomes Raven's bootstrap-v1
foundation must
be tested as both a compiler product and a compiler for real Raven workloads.
Self-compilation alone is not sufficient evidence.

The [staged bootstrap
procedure](../compiler/architecture/bootstrap-procedure.md) defines how this
foundation feeds Raven-native compiler API adoption and the later source port.

## Required gates

Run the following against the exact clean commit intended for release or use as
a bootstrap version:

```bash
scripts/test-baseline.sh
scripts/test-runtime-isolated.sh
FORCE_REBUILD=1 samples/build.sh -f net10.0
samples/run.sh
scripts/build-project-samples.sh
scripts/run-project-samples.sh
scripts/test-target-framework-matrix.sh
```

The aggregate project runner evaluates every project, executes every ordinary
`OutputType=Exe` project through `dotnet run --no-build` using the same
configuration and repository toolchain, and writes TSV and Markdown reports
beside the build report. Non-terminating, hardware, browser, and server projects
must have a reviewed entry in `samples/projects/run-classifications.tsv`; an
unlisted executable is always run and cannot silently become build-only.

The standalone and project sample sets are compatibility suites. Every
standalone sample and every executable project sample must both build and run
successfully. A build-only result is not a passing runtime gate. Keep failures
visible and classify intentional non-runnable projects explicitly; do not
silently remove a sample because it exposes a compiler regression. Server,
browser, hardware, library-only, and platform-specific projects may use a
documented unattended smoke substitute or remain build-only only when direct
execution would be unsafe, non-terminating, or meaningless. Every exclusion
must record its reason and is reviewed at the release freeze.

`scripts/build-project-samples.sh` uses the repository
`build/Raven.Language.targets` and the repository compiler host by default.
Use `--installed-toolchain` only when deliberately validating the separately
installed SDK. This distinction prevents a repository compiler DLL from being
combined accidentally with stale installed MSBuild targets.

## .NET SDK and target-framework matrix

Raven's SDK/compiler host runs on .NET 11, but it must select reference
assemblies, `Raven.Core`, emitted assembly scopes, and runtime dependencies for
the project target rather than for the host runtime.

`scripts/test-target-framework-matrix.sh` verifies this boundary by:

1. requiring an active .NET 11-or-newer SDK and recording its exact version;
2. building the repository compiler host for `net11.0`;
3. building Raven-authored `Raven.Core` and `Raven.Macros` for both `net10.0`
   and `net11.0`;
4. building representative `net10.0` and `net11.0` Raven projects with that
   same `net11.0` compiler host and the repository MSBuild targets; and
5. running both target families so bad framework identities or missing runtime
   dependencies fail at load or execution time.

The full project sample build remains required in addition to this focused
matrix. The focused script makes the cross-target contract quick to reproduce;
the complete corpus supplies breadth.

## Representative sample IL gate

In addition to build-and-run coverage, IL-verify a reviewed set of
compiler-shaped standalone and project samples in Release configuration. The
set must cover at least generic records and unions, pattern-heavy control flow,
async/state-machine emission, macros, collections and extension methods, and
.NET interop. Add a sample when a new compiler defect reveals an emitted shape
not represented by the set.

Treat failures as clusters rather than isolated sample incidents. Reduce each
failure and determine the earliest divergent boundary using the regression
isolation procedure in the
[bootstrap procedure](../compiler/architecture/bootstrap-procedure.md). If
several samples expose the same emitter or lowering invariant, prefer a shared
repair or bounded refactoring over per-sample compiler patches. The release
record identifies every verified assembly and its target framework.

## Bootstrap defect and backport handling

When porting exposes a new issue, record it in the porting ledger and classify
it before choosing a branch:

- Keep compiler corrections discovered during bootstrap v2 forward-only by
  default.
- Backport only when a reduced reproduction proves that the frozen v1 compiler
  or Core cannot correctly rebuild Core or supply a required v2 dependency.
  Add focused coverage, publish a new immutable v1 foundation revision, and
  carry that correction forward without moving or replacing the original
  artifacts.
- Decide and document unclear language or public API behavior before aligning
  both implementations.
- Fix a Raven-port-only defect forward and add differential coverage; do not
  manufacture an unrelated bootstrap-v1 change.
- Isolate a bootstrap-only accommodation at an explicit version boundary.

Every accepted bootstrap version reruns the baseline, runtime suite, target matrix, and
sample compatibility suites. The recorded result includes the commit, SDK
version, target frameworks, immutable annotated bootstrap tag, corresponding
public release tag when applicable, and whether repository or installed
artifacts were used.
