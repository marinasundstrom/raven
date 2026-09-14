# Raven main stability audit — 2026-09-14

Audited main `ea6f3383b` independently of the pending delegate fix and neoCLR
experiment. SDK: `11.0.100-rc.1.26425.128`. The pending delegate work was stashed
before the audit; no compiler changes were made during the initial sample build.

- `scripts/test-baseline.sh`: 5,493 passed, zero failures/skips. This baseline
  excludes designated emission-heavy/development suites; it is not every test.
- `FORCE_REBUILD=1 samples/build.sh -f net10.0`: 173 samples compiled, zero failures.
- `samples/run.sh -f net10.0`: 172 eligible samples succeeded, zero failures.
  Existing archived/intentional build exclusions and the interactive run exclusion
  were retained; no failures were hidden by adding exclusions.
- `scripts/build-project-samples.sh`: 66 passed, five failed out of 71 projects.
  Four NanoFramework samples failed in custom-attribute emission. The MacCatalyst
  host failed its workload prerequisite: installed SDK requires Xcode 26.6, but
  this machine has 26.2. That environment failure remains visible.

## Compiler regression and correction

The general metadata-type preference introduced in `031b9aaaa` reached custom
attribute serialization. Reflection's CustomAttributeBuilder expects runtime types
for its value encoding, so string constructor lookup and enum-valued attribute
serialization mixed host and MetadataLoadContext types. Two ordinary-reference
regressions reproduced these failures before the fix.

Custom-attribute type resolution now uses an explicit usage distinct from target
signatures. It preserves the existing runtime-type serialization path, including
source-defined attributes; target signatures continue using metadata types.
This is a general correction, not a NanoFramework-specific branch. All 39 focused
attribute/metadata tests pass, including both new regressions. It does not claim
new support for serializing arbitrary target-only attributes.

After rebuilding the repository compiler driver, all four NanoFramework projects
compiled successfully (Blinky, DHT22/display, temperature, Wi-Fi/HTTP). The first
retry had reused the old driver and reproduced the same failures; it was not counted
as validation of the correction. The project runner passed all 38 eligible executable
projects, with 13 explicit build-only classifications and 20 non-executable projects.
No run failures occurred. These projects use the same unchanged application binaries
from the initial build; the four corrected NanoFramework images are build-only.

Together, initial and targeted rebuild results cover 70 successful project builds;
the MacCatalyst host is still blocked on its toolchain prerequisite. This is not a
single all-green aggregate build. No sample source or exclusion was changed.
Actual execution of NanoFramework images requires devices; compilation is not a
claim of hardware execution. The Xcode prerequisite needs a compatible build host
before the complete project build gate can be green.

The .NET 10/.NET 11 repository build/run matrix also passed after the correction.
The broad baseline/standalone results above are explicitly the pre-correction audit;
post-correction evidence is the 39 focused tests, four NanoFramework builds, project
runs and target matrix. No source or exclusion changes were needed in the samples.
