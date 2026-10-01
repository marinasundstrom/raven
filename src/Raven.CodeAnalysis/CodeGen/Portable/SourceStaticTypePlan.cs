using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// The source type shape admitted by the current native backend. Symbol identity is
// retained for member ownership; names describe metadata, never reference equality.
internal sealed record SourceStaticTypePlan(INamedTypeSymbol Symbol, string Namespace, string Name)
{
    internal Accessibility Visibility => Symbol.DeclaredAccessibility;

    internal string FullName => Namespace.Length == 0 ? Name : Namespace + "." + Name;

    internal static bool TryCreate(INamedTypeSymbol type, out SourceStaticTypePlan? plan, EmissionCapabilities? capabilities = null)
    {
        plan = null;
        if (capabilities is not null && (!capabilities.Allows(EmissionDeclarationKind.StaticType) || !capabilities.AllowsTypeVisibility(type.DeclaredAccessibility))) return false;
        if (type.TypeKind != TypeKind.Class || !type.IsStatic || type.DeclaredAccessibility is not (Accessibility.Public or Accessibility.Internal) ||
            type.Arity != 0 || type.ContainingType is not null)
            return false;
        var fullName = type.ToFullyQualifiedMetadataName();
        var typeNamespace = type.ContainingNamespace.IsGlobalNamespace ? "" : fullName[..^(type.MetadataName.Length + 1)];
        plan = new(type, typeNamespace, type.MetadataName);
        return true;
    }

    internal TType Define<TType>(IStaticTypeDefinitionBuilder<TType> builder) => builder.DefineType(this);
}

internal interface IStaticTypeDefinitionBuilder<TType>
{
    TType DefineType(SourceStaticTypePlan plan);
}
