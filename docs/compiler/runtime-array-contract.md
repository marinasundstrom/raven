# Target array covariance policy

`CompilationOptions.AllowArrayCovariance` defaults to `true`, retaining Raven's
ordinary CLR array-conversion behavior. A target that requires invariant mutable
arrays can select `false`, either in compilation options or through the evaluated
project property:

```xml
<RavenAllowArrayCovariance>false</RavenAllowArrayCovariance>
```

The semantic conversion classifier then rejects nonidentity element conversions
between arrays. This applies to assignment, arguments, returns and explicit casts,
including nested arrays. Exact element types and runtime-identity-equivalent
nullability annotations retain their existing rules. There is no new grammar or
special language-server diagnostic implementation: editor and compiler diagnostics
come from the same project compilation options and semantic model.

The policy is independent of the iteration interface names and whether a target
projects array iteration interfaces. It does not implement generic interface or
delegate variance, readonly arrays, or a new runtime representation. Casts from a
less-specific interface or Object still require the target runtime to validate the
actual allocation; this setting is not a replacement for runtime checks.

This opt-out deliberately differs from [C# array covariance](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/arrays#176-array-covariance),
which permits reference-element widening and checks writes at runtime. neoCLR uses
invariant mutable arrays and may later offer covariant read-only interfaces. Other
frameworks keep the default unless configured explicitly. The compiler change is
on the neoCLR experiment branch; existing installed tools need a newer build before
this property affects their behavior.
