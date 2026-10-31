namespace Raven.CodeAnalysis.Targets;

// Shared neoCLR semantic policy for the CLI bridge and bounded native metadata path.
// Broader native marker, type-handle and representation contracts remain incremental.
internal sealed class NeoClrCliRuntimeContract(CompilationOptions options) : CliRuntimeContract(options)
{
    protected override string ArraySliceMethodName => "GetSlice";

    protected override bool IsArraySliceResultType(ITypeSymbol resultType, ITypeSymbol elementType)
        => resultType is INamedTypeSymbol { IsRefLikeType: true, Arity: 1 } view &&
           view.OriginalDefinition.ToFullyQualifiedMetadataName() == "System.Slice`1" &&
           SymbolEqualityComparer.Default.Equals(view.TypeArguments[0], elementType);

    internal override string UnionInterfaceTypeName => "System.Runtime.CompilerServices.UnionValue";
    internal override string AsyncStateMachineTypeName => "System.Runtime.CompilerServices.AsyncStateMachine";
    internal override string RecordEquatableTypeName => "System.EquatableTo`1";
    internal override string TupleTypeName => "System.Tuple";
    internal override bool UsesStructuralTuples => true;
    internal override bool UsesStructuralFunctions => true;
    internal override bool UsesStructuralCompositions => true;
    internal override bool UsesInhabitedDelegateResults => true;
    internal override bool HasNativeSelfContract => Options.RuntimeSelfTypeContract is not null;

    internal override bool UsesSourceObjectRoot => Options.MetadataImportOptions?.UseSourceObjectRoot == true;

    protected override string? GetPlatformConfigurationError()
        => NeoClrCliProfile.GetConfigurationError(Options);
}
