# Macro capability declarations

This sample demonstrates declaration-level editor capabilities on a
function-shaped macro. `Show` remains the expansion entry point. Its
`keywords`, `tokenKinds`, `highlighting`, `fragments`, `symbols`, `completion`,
and `projection` clauses forward to ordinary namespace functions declared
beside the macro. It leaves only custom token-stream replacement unused because
the compiler's standard token stream is the natural fit for this DSL.

Namespace functions are the default composition model: they need no service
class and remain assembly implementation details. A larger macro can instead
name a qualified static function, such as `ShowServices.Complete`, without
changing the capability syntax or the underlying macro interfaces.

Run the sample with:

```bash
dotnet run --project MacroCapabilities.rvnproj --property WarningLevel=0
```

Expected output:

```text
capability functions
```
