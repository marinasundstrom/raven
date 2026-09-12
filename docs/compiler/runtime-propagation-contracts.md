# Alternative runtime propagation contracts

The opt-in `RuntimePropagationContract` compilation option selects a carrier
protocol by assembly simple name and fully qualified interface metadata name.
Workspace builds and the language server read the same project properties:

```xml
<RavenPropagationAssemblyName>PropagationContracts</RavenPropagationAssemblyName>
<RavenPropagationInterfaceType>Contracts.Propagatable`3</RavenPropagationInterfaceType>
```

With this option, postfix `?` requires exactly one matching self/output/residual
interface. Public nongeneric `TryGetOutput(out output)` and `TryGetResidual(out residual)`
return Boolean; a public static nongeneric `FromResidual(residual)` returns the carrier.
The compiler checks the self argument and compatible residuals. Missing or partial
configuration does not silently fall back to union naming conventions.

Selecting a contract changes carrier discovery, not exception policy. Raven's existing
.NET exception capture remains in place when a compatible error conversion exists.
A runtime that cannot support that behavior needs a separately validated policy;
renaming the interface does not disable exception lowering.

With no descriptor, existing .NET interface discovery and union conventions are
unchanged. Option copies and incremental semantic reuse account for the selection.
There is no new language syntax and no target-specific profile included here.
