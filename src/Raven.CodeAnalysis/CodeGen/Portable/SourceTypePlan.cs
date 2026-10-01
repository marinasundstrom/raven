using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// A bounded source type shape shared by target adapters. Symbol identity is
// retained for member ownership; names describe metadata, never reference equality.
internal sealed record SourceTypePlan(INamedTypeSymbol Symbol, string Namespace, string Name)
{
    internal bool IsStatic => Symbol.IsStatic;
    internal EmissionDeclarationKind DeclarationKind => IsStatic ? EmissionDeclarationKind.StaticType : EmissionDeclarationKind.RootClass;

    internal Accessibility Visibility => Symbol.DeclaredAccessibility;

    internal string FullName => Namespace.Length == 0 ? Name : Namespace + "." + Name;

    internal static bool TryCreate(INamedTypeSymbol type, out SourceTypePlan? plan, EmissionCapabilities? capabilities = null)
    {
        plan = null;
        var kind = type.IsStatic ? EmissionDeclarationKind.StaticType : EmissionDeclarationKind.RootClass;
        if (capabilities is not null && (!capabilities.Allows(kind) || !capabilities.AllowsTypeVisibility(type.DeclaredAccessibility))) return false;
        if (type.TypeKind != TypeKind.Class || type.DeclaredAccessibility is not (Accessibility.Public or Accessibility.Internal) ||
            type.Arity != 0 || type.ContainingType is not null ||
            !type.IsStatic && (type.IsAbstract || type is not SourceNamedTypeSymbol { IsRecord: false, IsSealedHierarchy: false } || !type.Interfaces.IsEmpty || type.BaseType?.SpecialType != SpecialType.System_Object))
            return false;
        var fullName = type.ToFullyQualifiedMetadataName();
        var typeNamespace = type.ContainingNamespace.IsGlobalNamespace ? "" : fullName[..^(type.MetadataName.Length + 1)];
        plan = new(type, typeNamespace, type.MetadataName);
        return true;
    }

    internal TType Define<TType>(ITypeDefinitionBuilder<TType> builder) => builder.DefineType(this);
}

internal interface ITypeDefinitionBuilder<TType>
{
    TType DefineType(SourceTypePlan plan);
}
