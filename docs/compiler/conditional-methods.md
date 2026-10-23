# Conditional diagnostics

Raven recognizes `System.Diagnostics.ConditionalAttribute` as a compiler convention.
This is shared language behavior for ordinary .NET and the neoCLR compiler line;
it does not require a Runtime Contract switch. The target supplies its own attribute
identity. No grammar, token, completion or syntax-highlighting changes are needed.

```raven
import System.Diagnostics.*

[Conditional("DEBUG")]
func EnableDiagnostics() {
    // Configure application diagnostics here.
}

EnableDiagnostics()
```

A call is retained when the **caller's syntax tree** defines any of the method's
conditional symbols (`ParseOptions.PreprocessorSymbolNames`, or compiler `--define`).
Otherwise the call, receiver and arguments are omitted before control-flow lowering.
Arguments are still bound and checked, and semantic symbol information is retained.
The method and its attributes remain in metadata, so a separately compiled caller
makes its own decision. Reflection can still invoke the method. Multiple conditions
are alternatives; symbol matching is case-sensitive. Overrides inherit base-method
conditions, but cannot introduce their own condition.

Conditional methods return unit/void, cannot have out parameters or implement
interface members, and cannot be converted to delegates. `RAV0506` reports invalid
conditional declarations and delegate conversions. Conditional attribute classes
remain visible to semantic queries; their applications are omitted from CLI metadata
when no condition is defined at the attribute application. Conditions on attribute
base classes are inherited.

This follows [.NET ConditionalAttribute](https://learn.microsoft.com/en-us/dotnet/api/system.diagnostics.conditionalattribute?view=net-10.0).
`DEBUG` is an ordinary symbol, not an optimization setting. Conditional diagnostics
must not perform work required for application correctness. Debug symbols and
optimization separately affect stack-trace detail; they do not imply stack capture.

The neoCLR runtime integration and any native metadata restrictions are tracked in
[the native bridge document](neoclr-cli-bridge.md). .NET execution tests do not prove
.NET Framework, NanoFramework or native runtime execution.

Detached syntax used by macro-fragment semantic queries inherits caller symbols
from the owning semantic model when it has no syntax tree of its own. This keeps
conditional-call checks usable during hover and incomplete macro editing.
