namespace Raven.CodeAnalysis.Targets;

// The supported configuration surface of the experimental CLI bridge, not a
// native loader/backend or a claim that every language feature is supported.
internal static class NeoClrCliProfile
{
    internal const string CoreAssemblyName = "NeoCLR.CoreProbe";
    internal static RuntimeUnitContract Unit => new(CoreAssemblyName, "System.Void");

    internal static CompilationOptions CreateOptions() =>
        new CompilationOptions(OutputKind.ConsoleApplication,
            targetPlatform: TargetPlatform.NeoCLR,
            metadataImportOptions: new MetadataImportOptions(CoreAssemblyName),
            targetCoreAssemblyName: CoreAssemblyName,
            runtimeUnitContract: Unit,
            allowArrayCovariance: false,
            allowNullableValueTypes: false,
            frameworkProjectionMode: FrameworkProjectionMode.None)
        .WithRuntimeIterationContract(new(CoreAssemblyName,
            "System.Collections.Iterable`1", "System.Collections.Iterator`1",
            ArrayShapeTypeName: "System.Array`1"))
        .WithRuntimeDisposalContract(new(CoreAssemblyName, "System.Disposable", UseExceptionHandling: false))
        .WithRuntimePropagationContract(new(CoreAssemblyName, "System.Propagatable`3"))
        .WithRuntimeTypeOfContract(new(CoreAssemblyName,
            "System.Introspection.TypeInfo", "System.Runtime.RuntimeContext"))
        .WithGraphemeChar(true)
        .WithHeapAsyncStateMachines(true)
        .WithAsyncCancellationPropagation(true)
        .WithAsyncExceptionCapture(false);

    internal static string? GetConfigurationError(CompilationOptions options)
    {
        if (options.TargetCoreAssemblyName != CoreAssemblyName ||
            options.MetadataImportOptions?.CoreAssemblyName != CoreAssemblyName ||
            options.RuntimeUnitContract is not { TypeName: "System.Void", MapClrVoidToUnit: false } unit ||
            string.IsNullOrWhiteSpace(unit.AssemblyName))
        {
            return "the neoCLR CLI profile requires explicit NeoCLR.CoreProbe metadata and emission cores " +
                "and an explicitly owned System.Void unit contract; start with CompilationOptions.NeoCLR";
        }

        return null;
    }
}
