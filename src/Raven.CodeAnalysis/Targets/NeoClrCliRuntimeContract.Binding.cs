namespace Raven.CodeAnalysis.Targets;

internal sealed partial class NeoClrCliRuntimeContract
{
    internal override bool SupportsFunctionTypeMethods => true;
    internal override bool SupportsFunctionReturnTypeProjection => true;
    internal override bool SupportsIntersectionTypes => true;
    internal override bool SupportsCharKeyword => false;
    protected override string CharacterMetadataName => "System.Character";

    internal override (ITypeSymbol Type, INamedTypeSymbol? Definition) NormalizeResolvedType(
        Compilation compilation, ITypeSymbol type, INamedTypeSymbol? definition)
    {
        // Namespace spelling must select the explicitly owned Object, even when
        // namespace lookup encounters the bootstrap declaration first.
        if (Options.MetadataImportOptions is { ObjectAssemblyName: not null } rootImports &&
            type is INamedTypeSymbol { Arity: 0, ContainingType: null } objectType &&
            objectType.ContainingAssembly?.Name == rootImports.CoreAssemblyName &&
            objectType.ToFullyQualifiedMetadataName() == "System.Object")
        {
            type = definition = compilation.GetSpecialType(SpecialType.System_Object);
            if (type.TypeKind == TypeKind.Error)
                return (type, definition);
        }

        // Source handles implement runtime intrinsics while their signatures
        // retain the canonical bootstrap identity.
        if (Options.MetadataImportOptions?.SourcePrimitiveTypes.Contains(SpecialType.System_RuntimeTypeHandle) == true &&
            type is INamedTypeSymbol { Arity: 0, ContainingType: null } sourceHandle &&
            SymbolEqualityComparer.Default.Equals(sourceHandle.ContainingAssembly, compilation.Assembly) &&
            sourceHandle.ToFullyQualifiedMetadataName() == "System.RuntimeTypeHandle")
        {
            type = definition = compilation.GetSpecialType(SpecialType.System_RuntimeTypeHandle);
            if (type.TypeKind == TypeKind.Error)
                return (type, definition);
        }

        // Keep the nominal definition while projecting the selected unit's value
        // type, including bootstrap declarations found through merged namespaces.
        if (Options.RuntimeUnitContract is { } unitContract &&
            unitContract.AssemblyName != Options.TargetCoreAssemblyName &&
            compilation.ResolveRuntimeUnitType() is { } selectedUnit &&
            (SymbolEqualityComparer.Default.Equals(type, selectedUnit) ||
             unitContract.TypeName == "System.Void" &&
             type is INamedTypeSymbol { Arity: 0, ContainingType: null, IsValueType: true } bootstrapUnit &&
             bootstrapUnit.ToFullyQualifiedMetadataName() == unitContract.TypeName &&
             bootstrapUnit.ContainingAssembly?.Name == Options.MetadataImportOptions?.CoreAssemblyName))
            type = compilation.GetSpecialType(SpecialType.System_Unit);

        return base.NormalizeResolvedType(compilation, type, definition);
    }

    internal override ITypeSymbol GetPointerElementType(Compilation compilation, ITypeSymbol type)
    {
        // Existing CLI bridge rule: a pointer to the selected Void unit is PTR
        // VOID, not a pointer to its nominal storage carrier.
        if (Options.RuntimeUnitContract is { TypeName: "System.Void", MapClrVoidToUnit: false } &&
            (type.SpecialType == SpecialType.System_Unit ||
             SymbolEqualityComparer.Default.Equals(type, compilation.ResolveRuntimeUnitType())))
            return compilation.GetSpecialType(SpecialType.System_Void);
        return type;
    }

    internal override INamedTypeSymbol? GetSourceMemberLookupType(Compilation compilation, INamedTypeSymbol type)
    {
        if (Options.MetadataImportOptions?.SourcePrimitiveTypes.Contains(type.SpecialType) != true)
            return type;

        // Member lookup follows the selected source implementation; signature and
        // expression storage keep the bootstrap's canonical primitive identity.
        return compilation.Assembly.GetTypeByMetadataName(type.ToFullyQualifiedMetadataName());
    }
}
