using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// A bounded source type shape shared by target adapters. Symbol identity is
// retained for member ownership; names describe metadata, never reference equality.
internal sealed record SourceTypePlan(INamedTypeSymbol Symbol, string Namespace, string Name)
{
    internal INamedTypeSymbol? MetadataOwner => GetMetadataOwner(Symbol);
    private static INamedTypeSymbol? GetMetadataOwner(INamedTypeSymbol type) =>
        type.OriginalDefinition is SourceUnionCaseTypeSymbol unionCase ? unionCase.MetadataContainingType : type.ContainingType;
    internal bool IsExtensionContainer => Symbol.OriginalDefinition is SourceNamedTypeSymbol { IsExtensionDeclaration: true };
    internal bool IsStatic => IsStaticContainer(Symbol);
    internal static bool IsStaticContainer(INamedTypeSymbol type) => type.IsStatic ||
        type.OriginalDefinition is SourceNamedTypeSymbol { IsExtensionDeclaration: true };
    internal bool IsClosedHierarchy => Symbol.IsSealedHierarchy;
    internal bool IsEnum => Symbol.TypeKind == TypeKind.Enum;
    internal bool IsValueType => Symbol.TypeKind is TypeKind.Struct or TypeKind.Enum;
    internal EmissionDeclarationKind DeclarationKind => IsEnum ? EmissionDeclarationKind.Enum : IsValueType ? EmissionDeclarationKind.ValueType : IsStatic ? EmissionDeclarationKind.StaticType : EmissionDeclarationKind.RootClass;

    internal INamedTypeSymbol? ClassBase => !IsStatic && !IsValueType && Symbol.BaseType is { SpecialType: not SpecialType.System_Object } parent ? parent : null;

    internal Accessibility Visibility => Symbol.DeclaredAccessibility;

    internal string FullName => Namespace.Length == 0 ? Name : Namespace + "." + Name;

    internal static bool TryCreate(INamedTypeSymbol type, out SourceTypePlan? plan, EmissionCapabilities? capabilities = null)
    {
        plan = null;
        if (type.TypeKind == TypeKind.Enum)
        {
            if (capabilities?.Allows(EmissionDeclarationKind.Enum) != true || type.EnumUnderlyingType?.SpecialType != SpecialType.System_Int32 ||
                type.ContainingType is not null || type.Arity != 0 || !capabilities.AllowsTypeVisibility(type.DeclaredAccessibility)) return false;
            plan = new(type, type.ContainingNamespace?.ToMetadataName() ?? "", type.MetadataName);
            return true;
        }

        var isStatic = IsStaticContainer(type);
        if (!type.IsStatic && isStatic && capabilities?.AllowsLoweredExtensionCalls != true) return false;
        var isValue = type.TypeKind == TypeKind.Struct;
        var closedFamily = type.IsSealedHierarchy && type.IsAbstract && !isValue && type.Arity == 0 && type.ContainingType is null &&
            type.BaseType?.SpecialType == SpecialType.System_Object && capabilities?.AllowsClosedClassFamilies == true;
        var kind = isValue ? EmissionDeclarationKind.ValueType : isStatic ? EmissionDeclarationKind.StaticType : EmissionDeclarationKind.RootClass;
        if (capabilities is not null && (!capabilities.Allows(kind) || !capabilities.AllowsTypeVisibility(type.DeclaredAccessibility))) return false;
        if (type.Arity > 0 && (capabilities is not null && !(isStatic ? capabilities.AllowsGenericStaticOwners : capabilities.AllowsGenericClassOwners) ||
            ((INamedTypeSymbol)type.OriginalDefinition).TypeParameters.Any(p =>
                (p.ConstraintKind & ~(TypeParameterConstraintKind.TypeConstraint | TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.ValueType | TypeParameterConstraintKind.Constructor)) != 0 ||
                (p.ConstraintKind & (TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.ValueType | TypeParameterConstraintKind.Constructor)) != 0 && capabilities is not null && !capabilities.AllowsSpecialTypeConstraints ||
                !p.ConstraintTypes.IsEmpty && (capabilities is not null && !capabilities.AllowsNominalTypeBounds || p.ConstraintTypes.Length != 1 ||
                    p.ConstraintTypes[0] is not INamedTypeSymbol { Arity: 0, IsStatic: false } bound || !TryCreate(bound, out _))))) return false;
        if (isValue && (capabilities?.Allows(EmissionDeclarationKind.ValueType) != true || !type.Interfaces.IsEmpty && capabilities?.Allows(EmissionDeclarationKind.ValueInterfaceImplementation) != true ||
            type.OriginalDefinition is SourceNamedTypeSymbol { IsRefLikeType: true } ||
            ((INamedTypeSymbol)type.OriginalDefinition).TypeParameters.Any(p => p.ConstraintKind != TypeParameterConstraintKind.None))) return false;
        if (type.TypeKind is not (TypeKind.Class or TypeKind.Struct) || type.DeclaredAccessibility is not (Accessibility.Public or Accessibility.Internal) ||
            GetMetadataOwner(type) is { } parent && (capabilities?.Allows(EmissionDeclarationKind.NestedType) != true ||
                parent.Arity != 0 || isStatic || !isValue && type.Arity != 0 || !TryCreate(parent, out _, capabilities)) ||
            !isStatic && (type.IsAbstract && !closedFamily || type.IsSealedHierarchy && !closedFamily || type.OriginalDefinition is not SourceNamedTypeSymbol { IsRecord: false } || type.BaseType?.SpecialType != (isValue ? SpecialType.System_ValueType : SpecialType.System_Object) &&
                !(capabilities?.AllowsLocalClassInheritance == true && !isValue && type.Arity == 0 && type.ContainingType is null &&
                  type.BaseType is { Arity: 0, ContainingType: null } baseType && SymbolEqualityComparer.Default.Equals(baseType.ContainingAssembly, type.ContainingAssembly) && TryCreate(baseType, out _, capabilities))))
            return false;
        // Check relationship identity here. Arguments are mapped by the adapter; recursively
        // admitting their source owners would loop for shapes such as C<T> : I<C<T>>.
        if (!type.Interfaces.IsEmpty && (type.Arity != 0 && capabilities is not null && !capabilities.AllowsConstructedInterfaceImplementations || capabilities is not null && !capabilities.Allows(EmissionDeclarationKind.InterfaceImplementation) ||
            type.Interfaces.Any(i => !SourceInterfacePlan.HasSupportedRelationship(i, type.ContainingAssembly, capabilities) ||
                i.Arity != 0 && capabilities is not null && !capabilities.AllowsConstructedInterfaceImplementations))) return false;
        if (GetMetadataOwner(type) is not null)
        {
            plan = new(type, "", type.MetadataName);
            return true;
        }
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
