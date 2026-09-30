namespace Raven.CodeAnalysis.Targets;

// Explicit neoCLR semantics under the temporary CLI loader/emitter. A native
// contract must replace CLI marker, type-handle and representation assumptions.
internal sealed class NeoClrCliRuntimeContract(CompilationOptions options) : CliRuntimeContract(options)
{
    internal override string TupleTypeName => "System.Tuple";
    internal override bool UsesInhabitedDelegateResults => true;
    internal override bool HasNativeSelfContract => Options.RuntimeSelfTypeContract is not null;

    protected override string? GetPlatformConfigurationError()
        => NeoClrCliProfile.GetConfigurationError(Options);
}
