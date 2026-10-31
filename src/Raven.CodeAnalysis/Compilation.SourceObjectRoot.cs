using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private SourceNamedTypeSymbol? _sourceObjectRoot;
    internal SpecialType GetSourceCoreSpecialType(INamedTypeSymbol type)
    {
        if (Options.TargetPlatform != TargetPlatform.NeoCLR || Options.MetadataImportOptions?.UseSourceCore != true ||
            type.ContainingType is not null || type.ContainingAssembly?.Name != AssemblyName)
            return SpecialType.None;
        var name = type.ToFullyQualifiedMetadataName();
        foreach (var special in Enum.GetValues<SpecialType>())
            if (special is not (SpecialType.None or SpecialType.System_Unit) &&
                _target.RuntimeContract.GetSpecialTypeMetadataName(special) == name)
                return special;
        return SpecialType.None;
    }
    internal bool UsesSourceObjectRoot => _target.RuntimeContract.UsesSourceObjectRoot;
    internal bool IsSourceObjectRoot(INamedTypeSymbol type)
        => UsesSourceObjectRoot && ReferenceEquals(type, _sourceObjectRoot);

    internal string? GetSourceObjectRootError()
    {
        if (!UsesSourceObjectRoot)
            return null;
        EnsureSourceDeclarationsComplete();
        return _sourceObjectRoot is
        {
            DeclaredAccessibility: Accessibility.Public, IsAbstract: true,
            IsStatic: false, BaseType: null
        } root &&
            !root.GetMembers().OfType<IFieldSymbol>().Any(field => !field.IsStatic)
            ? null : "source Object ownership requires a public abstract fieldless nongeneric System.Object class without a base";
    }

    // Declaration shells are complete, but no member signatures have been bound.
    // Do not put provisional bootstrap Object identities in the selected-root cache.
    private void SelectSourceObjectRoot()
    {
        if (!UsesSourceObjectRoot || _sourceTypeDeclarationsDeclared)
            return;
        _sourceObjectRoot = Assembly.GetTypeByMetadataName("System.Object") as SourceNamedTypeSymbol;
        if (_sourceObjectRoot is not { TypeKind: TypeKind.Class, Arity: 0, ContainingType: null })
        {
            _sourceObjectRoot = null;
            return;
        }
        _sourceObjectRoot.SetBaseType(null);
        foreach (var type in _declaredTypeSymbols.Values)
            if (!ReferenceEquals(type, _sourceObjectRoot) && type.BaseType?.SpecialType == SpecialType.System_Object)
                type.SetBaseType(_sourceObjectRoot);
        if (Options.MetadataImportOptions?.UseSourceCore == true)
            foreach (var type in _declaredTypeSymbols.Values)
                if (!ReferenceEquals(type, _sourceObjectRoot) && type.BaseType is null or { TypeKind: TypeKind.Error })
                    type.SetBaseType(type.TypeKind switch
                    {
                        TypeKind.Struct => Assembly.GetTypeByMetadataName("System.ValueType"),
                        TypeKind.Enum => Assembly.GetTypeByMetadataName("System.Enum"),
                        TypeKind.Class => _sourceObjectRoot,
                        _ => type.BaseType
                    });
    }
}
