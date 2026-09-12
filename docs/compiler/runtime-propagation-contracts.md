# Alternative runtime propagation contracts

The experimental `RuntimePropagationContract` compilation option selects a carrier
protocol by assembly simple name and fully qualified interface metadata name.
Workspace builds and the language server read the same project properties:

```xml
<RavenPropagationAssemblyName>NeoCLR.CoreProbe</RavenPropagationAssemblyName>
<RavenPropagationInterfaceType>System.Propagatable`3</RavenPropagationInterfaceType>
```

With this option, postfix `?` requires exactly one matching self/output/residual
interface. Public nongeneric `TryGetOutput(out output)` and `TryGetResidual(out residual)`
return Boolean; a public static nongeneric `FromResidual(residual)` returns the carrier.
The compiler checks the self argument and compatible residuals. Missing or partial
configuration does not silently fall back to union naming conventions.

Selected-target propagation handles carrier results only. It does not synthesize
exception capture around the operand. Faults remain failures; they are not converted
into residuals. This differs from Raven's default .NET propagation lowering, which
can capture compatible exceptions. Explicit `try` syntax remains separate and is not
made portable by selecting this contract. A target importer must reject unsupported
exception handling and validate the admitted metadata/operations.

With no descriptor, existing .NET interface discovery, union conventions and exception
behavior are unchanged. Option copies and incremental semantic reuse account for the
selection. There is no new language syntax. This is an experimental target hook, not
an automatic retargeting of arbitrary .NET programs.

The neoCLR bridge currently demonstrates `Result<int, OverflowError>` success and
early error return. The installed `0.1.12-neoclr.3` tools predate this feature; use a
source build of the experimental branch. Option/Void residual projection is pending.
