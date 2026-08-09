# Opt-in `is not null` narrowing

This sample enables Raven's compatibility-only null-check narrowing for code
that consumes null-oriented .NET contracts:

```xml
<EnableIsNotNullNarrowing>true</EnableIsNotNullNarrowing>
```

With that option, a direct `value is not null` condition treats a stable local
or parameter as its non-null type inside the true branch. The declared type
remains nullable, and the value is nullable again outside the branch. Mutable
locals and properties are deliberately not narrowed because their storage can
change between the check and use.

Raven's canonical forms bind a separate non-null value and do not require the
compatibility option:

```raven
if let text: string = value {
    text.Trim()
}

if value is string text {
    text.Trim()
}
```

The compatibility option is disabled by default. It does not enable general
nullable flow analysis across assignments, loops, early exits, equality
operators, or arbitrary boolean expressions.

This is not Raven's model for domain absence. Use `Option<T>` when a value may
meaningfully be present or absent; reserve `T?` for null references in interop
or storage contracts. Nullable boundaries still support Raven's regular tools,
including patterns and conditional access; enabling this option only adds the
more C#-like guarded-storage experience.

Run the sample with:

```bash
dotnet run --project samples/projects/null-narrowing-compatibility/NullNarrowingCompatibility.rvnproj
```
