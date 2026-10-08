namespace Raven.CodeAnalysis.Targets;

internal sealed class DotNetRuntimeContract(CompilationOptions options) : CliRuntimeContract(options)
{
    // Controlled callers still use the historical probe-core trigger without
    // selecting NeoCLR. Preserve that transport ABI without enabling native Self.
    internal override string TupleTypeName => NeoClrCliCompatibility.UsesLegacyContract(Options)
        ? "System.Tuple" : "System.ValueTuple";

    internal override string AsyncStateMachineTypeName => NeoClrCliCompatibility.UsesLegacyContract(Options)
        ? "System.Runtime.CompilerServices.AsyncStateMachine" : base.AsyncStateMachineTypeName;

    internal override string UnionInterfaceTypeName => NeoClrCliCompatibility.UsesLegacyContract(Options)
        ? "System.Runtime.CompilerServices.UnionValue" : base.UnionInterfaceTypeName;

    internal override bool UsesInhabitedDelegateResults => NeoClrCliCompatibility.UsesLegacyContract(Options);
    internal override bool HasNativeSelfContract => false;

    protected override string? GetPlatformConfigurationError()
        => Options.RuntimeSelfTypeContract is not null
            ? "native Self requires TargetPlatform.NeoCLR; it is not supported by the .NET target"
            : null;
}
