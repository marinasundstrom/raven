# Target ownership in tests

Tests must make target-specific expectations identifiable and selectable. Target
ownership is independent of feature, compiler layer and test cost. Apply this
convention to new coverage and when changing existing target-sensitive tests;
classification of the historical suite is incremental, not yet exhaustive.

| Trait | Meaning |
|---|---|
| `[Trait("Target", "Shared")]` | A target-independent compiler contract, such as syntax or common semantic API behavior |
| `[Trait("Target", "DotNet")]` | .NET-specific policy, representation, metadata or execution |
| `[Trait("Target", "NeoCLR")]` | neoCLR-specific policy, representation, metadata or execution |

A shared test may use ordinary .NET references as a convenient fixture. That does
not establish behavior on every target. Conversely, a neoCLR semantic test can
run in the .NET test host without executing a native program. State the fixture
and boundary accurately; only a runtime invocation qualifies runtime execution.
Record interpreter/JIT/AOT and target framework in execution receipts as applicable.

Use explicit target names in target-dependent test methods or theory arguments.
Avoid boolean arguments such as `native: true` in test reports. Where expectations
differ, prefer separately attributed .NET and neoCLR theories calling a shared
fixture helper. This keeps each case selectable without duplicating setup.
A single boundary test that deliberately compares both targets may carry both
traits; each target filter then runs that complete comparison. Never assign both
traits solely because a general compiler behavior is intended to be portable.

Keep shared regression fixtures independent of neoCLR runtime libraries. Put
native metadata/runtime fixture setup alongside native tests or in explicit
native fixture helpers. A class-level target trait is appropriate only when all
its tests share that ownership. Do not classify an entire mixed class by name.

Examples:

```csharp
[Theory]
[Trait("Target", "DotNet")]
[InlineData("Samples")]
[InlineData("")]
public void DotNetAssemblyMemberSidecarsUseCarrierIds(string namespaceName)
    => VerifyAssemblyMemberSidecars(TargetPlatform.DotNet, "NamespaceMembers.", namespaceName);

[Theory]
[Trait("Target", "NeoCLR")]
[InlineData("Samples")]
[InlineData("")]
public void NeoClrAssemblyMemberSidecarsUseNamespaceIds(string namespaceName)
    => VerifyAssemblyMemberSidecars(TargetPlatform.NeoCLR, "", namespaceName);
```

Select classified tests using ordinary xUnit traits:

```sh
dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj --filter 'Target=DotNet' /property:WarningLevel=0
dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj --filter 'Target=NeoCLR' /property:WarningLevel=0
```

These filters currently select only classified coverage. They do not replace the
baseline, feature suites, native probe harnesses or runtime qualification. Broad
renaming/moving of historical tests belongs in a separate post-merge cleanup,
with discovered test counts and suite membership checked before and after.

For example, intersection parser tests are shared syntax coverage; .NET generic
constraint emission tests are `.NET` runtime coverage, not proof of intersection
type support. Native intersection storage/semantic tests are neoCLR coverage.
Structural union language rules are shared even when representation tests differ.
