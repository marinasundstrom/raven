# Release and bootstrap compatibility gates

The release candidate that becomes Raven's stage-0 bootstrap foundation must
be tested as both a compiler product and a compiler for real Raven workloads.
Self-compilation alone is not sufficient evidence.

## Required gates

Run the following against the exact clean commit intended for release or use as
a bootstrap stage:

```bash
scripts/test-baseline.sh
scripts/test-runtime-isolated.sh
FORCE_REBUILD=1 samples/build.sh -f net10.0
samples/run.sh
scripts/build-project-samples.sh
scripts/test-target-framework-matrix.sh
```

The standalone and project sample sets are compatibility suites. Keep failures
visible and classify intentional non-runnable projects explicitly; do not
silently remove a sample because it exposes a compiler regression. Server,
browser, hardware, and platform-specific projects may remain build-only gates
when unattended execution would be unsafe or non-terminating.

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

## Bootstrap defect and backport handling

When porting exposes a new issue, record it in the porting ledger and classify
it before choosing a branch:

- Fix a stage-0 compiler/runtime defect on the maintained pre-bootstrap line,
  add focused coverage there, and carry the corrected behavior forward.
- Decide and document unclear language or public API behavior before aligning
  both implementations.
- Fix a Raven-port-only defect forward and add differential coverage; do not
  manufacture an unrelated stage-0 change.
- Isolate a bootstrap-only accommodation at an explicit stage boundary.

Every accepted stage reruns the baseline, runtime suite, target matrix, and
sample compatibility suites. The recorded result includes the commit, SDK
version, target frameworks, and whether repository or installed artifacts were
used.
