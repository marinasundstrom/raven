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
        if (type.Arity > 0 && (capabilities is not null && !(type.IsStatic ? capabilities.AllowsGenericStaticOwners : capabilities.AllowsGenericClassOwners) ||
            ((INamedTypeSymbol)type.OriginalDefinition).TypeParameters.Any(p =>
                (p.ConstraintKind & ~(TypeParameterConstraintKind.TypeConstraint | TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.ValueType | TypeParameterConstraintKind.Constructor)) != 0 ||
                (p.ConstraintKind & (TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.ValueType | TypeParameterConstraintKind.Constructor)) != 0 && capabilities is not null && !capabilities.AllowsSpecialTypeConstraints ||
                !p.ConstraintTypes.IsEmpty && (capabilities is not null && !capabilities.AllowsNominalTypeBounds || p.ConstraintTypes.Length != 1 ||
                    p.ConstraintTypes[0] is not INamedTypeSymbol { Arity: 0, IsStatic: false } bound || !TryCreate(bound, out _))))) return false;
        if (type.TypeKind != TypeKind.Class || type.DeclaredAccessibility is not (Accessibility.Public or Accessibility.Internal) ||
            type.ContainingType is not null ||
            !type.IsStatic && (type.IsAbstract || type.OriginalDefinition is not SourceNamedTypeSymbol { IsRecord: false, IsSealedHierarchy: false } || type.BaseType?.SpecialType != SpecialType.System_Object))
            return false;
        // Check relationship identity here. Arguments are mapped by the adapter; recursively
        // admitting their source owners would loop for shapes such as C<T> : I<C<T>>.
        if (!type.Interfaces.IsEmpty && (type.Arity != 0 && capabilities is not null && !capabilities.AllowsConstructedInterfaceImplementations || capabilities is not null && !capabilities.Allows(EmissionDeclarationKind.InterfaceImplementation) ||
            type.Interfaces.Any(i => !SourceInterfacePlan.HasSupportedRelationship(i, type.ContainingAssembly, capabilities) ||
                i.Arity != 0 && capabilities is not null && !capabilities.AllowsConstructedInterfaceImplementations))) return false;
        var fullName = type.ToFullyQualifiedMetadataName();
        // MetadataName may already be qualified on synthesized owners. Split the
        // normalized full name instead of subtracting a potentially qualified name.
        var separator = fullName.LastIndexOf('.');
        plan = new(type, separator < 0 ? "" : fullName[..separator], fullName[(separator + 1)..]);
        return true;
    }

    internal TType Define<TType>(ITypeDefinitionBuilder<TType> builder) => builder.DefineType(this);
}

internal interface ITypeDefinitionBuilder<TType>
{
    TType DefineType(SourceTypePlan plan);
}
