namespace Raven.CodeAnalysis.Targets;

// Shared neoCLR semantic policy for the CLI bridge and bounded native metadata path.
// Broader native marker, type-handle and representation contracts remain incremental.
internal sealed class NeoClrCliRuntimeContract(CompilationOptions options) : CliRuntimeContract(options)
{
    internal override string UnionInterfaceTypeName => "System.Runtime.CompilerServices.UnionValue";
    internal override string AsyncStateMachineTypeName => "System.Runtime.CompilerServices.AsyncStateMachine";
    internal override string TupleTypeName => "System.Tuple";
    internal override bool UsesInhabitedDelegateResults => true;
    internal override bool HasNativeSelfContract => Options.RuntimeSelfTypeContract is not null;

    internal override bool UsesSourceObjectRoot => Options.MetadataImportOptions?.UseSourceObjectRoot == true;

    protected override string? GetPlatformConfigurationError()
        => NeoClrCliProfile.GetConfigurationError(Options);
}
