using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Interface signatures are separate from callable body plans. Concrete static
// helpers are admitted only by an explicit target capability.
internal sealed record SourceInterfaceMethod(MethodSymbol Symbol, CallableSignature Signature);
internal sealed record SourceInterfaceProperty(PropertySymbol Symbol, EmissionType Type);
internal sealed record SourceInterfacePlan(NamedTypeSymbol Symbol, string Namespace, string Name,
    ImmutableArray<SourceInterfaceMethod> Methods, ImmutableArray<SourceInterfaceProperty> Properties,
    ImmutableArray<NamedTypeSymbol> BaseInterfaces)
{
    // Identity admission must not recursively inspect members: interface signatures
    // can refer back to their own definition or to another interface.
    internal static bool HasSupportedIdentity(NamedTypeSymbol type, EmissionCapabilities? capabilities = null) =>
        type.TypeKind == TypeKind.Interface && type.ContainingType is null &&
        type.DeclaredAccessibility is Accessibility.Public or Accessibility.Internal &&
        type.DeclaringSyntaxReferences.Length == 1 && type.DeclaringSyntaxReferences[0].GetSyntax() is InterfaceDeclarationSyntax &&
        ((NamedTypeSymbol)type.OriginalDefinition).TypeParameters.All(p => (p.Variance == VarianceKind.None || capabilities?.AllowsInterfaceVariance == true) &&
            p.ConstraintKind == TypeParameterConstraintKind.None && p.ConstraintTypes.IsEmpty);

    internal static bool HasSupportedRelationship(NamedTypeSymbol target, AssemblySymbol owner, EmissionCapabilities? capabilities)
    {
        if (SymbolEqualityComparer.Default.Equals(target.ContainingAssembly, owner)) return HasSupportedIdentity(target, capabilities);
        return capabilities?.AllowsExternalInterfaceDeclarations == true && target.TypeKind == TypeKind.Interface &&
            target.ContainingType is null && target.DeclaredAccessibility == Accessibility.Public &&
            ((NamedTypeSymbol)target.OriginalDefinition).TypeParameters.All(p => (p.Variance == VarianceKind.None || capabilities?.AllowsInterfaceVariance == true) &&
                p.ConstraintKind == TypeParameterConstraintKind.None && p.ConstraintTypes.IsEmpty);
    }

    internal static bool TryCreate(NamedTypeSymbol type, EmissionCapabilities capabilities, out SourceInterfacePlan? plan)
    {
        plan = null;
        if (type.TypeKind != TypeKind.Interface || type.ContainingType is not null ||
            !capabilities.Allows(EmissionDeclarationKind.Interface) || !capabilities.Allows(EmissionDeclarationKind.InterfaceMethod) ||
            !capabilities.AllowsTypeVisibility(type.DeclaredAccessibility) || !capabilities.AllowsMethodVisibility(Accessibility.Public) ||
            type.Arity > 0 && !capabilities.AllowsGenericInterfaceDeclarations ||
            type.TypeParameters.Any(p => p.Variance != VarianceKind.None && !capabilities.AllowsInterfaceVariance || p.ConstraintKind != TypeParameterConstraintKind.None || !p.ConstraintTypes.IsEmpty) ||
            type.DeclaringSyntaxReferences.Length != 1 || type.DeclaringSyntaxReferences[0].GetSyntax() is not InterfaceDeclarationSyntax syntax ||
            (!capabilities.AllowsCustomAttributes && syntax.AttributeLists.Count != 0) || syntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword) &&
                !(m.Kind == SyntaxKind.SealedKeyword && capabilities.AllowsClosedInterfaceFamilies && type.Arity == 0)))
            return false;
        if (!type.Interfaces.IsEmpty && (!capabilities.Allows(EmissionDeclarationKind.InterfaceInheritance) ||
            type.Interfaces.Any(b => !HasSupportedRelationship(b, type.ContainingAssembly, capabilities) || b.Arity != 0 && (!capabilities.AllowsConstructedInterfaceInheritance ||
                !CallableSignature.TryType(b, false, out var inherited, capabilities) || !capabilities.Allows(inherited))))) return false;
        bool AllowsModifier(SyntaxToken modifier) => modifier.Kind is SyntaxKind.PublicKeyword or SyntaxKind.AbstractKeyword ||
            modifier.Kind == SyntaxKind.StaticKeyword && capabilities.Allows(EmissionDeclarationKind.StaticInterfaceMethod);
        bool AllowsStaticBody(SyntaxTokenList modifiers) => type.Arity == 0 &&
            capabilities.Allows(EmissionDeclarationKind.StaticInterfaceMethodBody) &&
            modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword) &&
            modifiers.All(m => m.Kind is SyntaxKind.PublicKeyword or SyntaxKind.StaticKeyword);
        foreach (var member in syntax.Members)
        {
            if (member is MethodDeclarationSyntax helper && (helper.Body is not null || helper.ExpressionBody is not null) &&
                AllowsStaticBody(helper.Modifiers) && helper.ExplicitInterfaceSpecifier is null &&
                (capabilities.AllowsCustomAttributes || helper.AttributeLists.Count == 0)) continue;
            if (member is PropertyDeclarationSyntax factory && factory.Initializer is null &&
                AllowsStaticBody(factory.Modifiers) && factory.ExplicitInterfaceSpecifier is null &&
                (capabilities.AllowsCustomAttributes || factory.AttributeLists.Count == 0) &&
                (factory.ExpressionBody is not null || factory.AccessorList is { } bodies &&
                    bodies.Accessors.All(a => a.Kind is SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration &&
                        (a.Body is not null || a.ExpressionBody is not null) && a.Modifiers.Count == 0 &&
                        (capabilities.AllowsCustomAttributes || a.AttributeLists.Count == 0)))) continue;
            if (member is MethodDeclarationSyntax { Body: null, ExpressionBody: null } method && (capabilities.AllowsCustomAttributes || method.AttributeLists.Count == 0) &&
                method.ExplicitInterfaceSpecifier is null && method.Modifiers.All(AllowsModifier)) continue;
            if (member is PropertyDeclarationSyntax { ExpressionBody: null, Initializer: null, AccessorList: { } accessors } property &&
                (capabilities.AllowsCustomAttributes || property.AttributeLists.Count == 0) && property.ExplicitInterfaceSpecifier is null &&
                property.Modifiers.All(AllowsModifier) &&
                accessors.Accessors.All(a => a.Kind is SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration &&
                    a.Body is null && a.ExpressionBody is null && (capabilities.AllowsCustomAttributes || a.AttributeLists.Count == 0) && a.Modifiers.Count == 0)) continue;
            if (member is IndexerDeclarationSyntax { ExpressionBody: null, AccessorList: { } indexAccessors } indexer &&
                capabilities.Allows(EmissionDeclarationKind.InterfaceIndexer) && (capabilities.AllowsCustomAttributes || indexer.AttributeLists.Count == 0) && indexer.ExplicitInterfaceSpecifier is null &&
                indexer.Modifiers.All(m => m.Kind is SyntaxKind.PublicKeyword or SyntaxKind.AbstractKeyword) &&
                indexAccessors.Accessors.All(a => a.Kind is SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration &&
                    a.Body is null && a.ExpressionBody is null && (capabilities.AllowsCustomAttributes || a.AttributeLists.Count == 0) && a.Modifiers.Count == 0)) continue;
            if (member is OperatorDeclarationSyntax { Body: null, ExpressionBody: null } op &&
                capabilities.Allows(EmissionDeclarationKind.StaticInterfaceMethod) && (capabilities.AllowsCustomAttributes || op.AttributeLists.Count == 0) &&
                op.Modifiers.All(AllowsModifier)) continue;
            return false;
        }
        var methods = ImmutableArray.CreateBuilder<SourceInterfaceMethod>();
        var properties = ImmutableArray.CreateBuilder<SourceInterfaceProperty>();
        var seen = new HashSet<MethodSymbol>(SymbolEqualityComparer.Default);
        bool AddMethod(MethodSymbol method)
        {
            if (!seen.Add(method)) return true;
            if (method.IsStatic && !capabilities.Allows(EmissionDeclarationKind.StaticInterfaceMethod) || method.IsGenericMethod && (!capabilities.AllowsGenericInterfaceMethods || method.IsStatic || method.TypeParameters.Any(p => p.ConstraintKind != TypeParameterConstraintKind.None || !p.ConstraintTypes.IsEmpty)) || !method.IsAbstract && !(method.IsStatic && type.Arity == 0 && capabilities.Allows(EmissionDeclarationKind.StaticInterfaceMethodBody)) ||
                method.MethodKind is not (MethodKind.Ordinary or MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.UserDefinedOperator) ||
                method.DeclaredAccessibility != Accessibility.Public || !CallableSignature.TryType(method.ReturnType, true, out var result, capabilities)) return false;
            var parameters = ImmutableArray.CreateBuilder<EmissionType>();
            foreach (var parameter in method.Parameters)
            {
                if (parameter.RefKind != RefKind.None && (!capabilities.AllowsManagedReferences || parameter.RefKind is not (RefKind.Ref or RefKind.Out)) || parameter.HasExplicitDefaultValue && (!capabilities.AllowsParameterDefaults || !CallableSignature.SupportsParameterDefault(parameter)) || !CallableSignature.AllowsParameterArray(method, parameter, capabilities) ||
                    !CallableSignature.TryType(parameter.Type, false, out var value, capabilities) || !capabilities.Allows(value)) return false;
                parameters.Add(value with { IsByReference = parameter.RefKind != RefKind.None });
            }
            if (!capabilities.Allows(result)) return false;
            methods.Add(new(method, new(result, parameters.ToImmutable(), GenericParameterNames: method.TypeParameters.Select(p => p.Name).ToImmutableArray(), IsInstance: !method.IsStatic, DeclaringTypeArity: type.Arity,
                OutParameters: method.Parameters.Select((p, i) => (p, i)).Where(x => x.p.RefKind == RefKind.Out).Select(x => x.i).ToImmutableArray())));
            return true;
        }
        foreach (var member in type.GetMembers())
        {
            if (member is MethodSymbol method) { if (!AddMethod(method)) return false; }
            else if (member is PropertySymbol property && (!property.IsStatic || capabilities.Allows(EmissionDeclarationKind.StaticInterfaceMethod)) &&
                capabilities.Allows(property.IsIndexer ? EmissionDeclarationKind.InterfaceIndexer : EmissionDeclarationKind.InterfaceProperty) && property.DeclaredAccessibility == Accessibility.Public &&
                CallableSignature.TryType(property.Type, false, out var value, capabilities) && capabilities.Allows(value))
            {
                if (property.GetMethod is { } get && !AddMethod(get) || property.SetMethod is { } set && !AddMethod(set)) return false;
                properties.Add(new(property, value));
            }
            else return false;
        }
        var fullName = type.ToFullyQualifiedMetadataName();
        plan = new(type, type.ContainingNamespace.IsGlobalNamespace ? "" : fullName[..^(type.MetadataName.Length + 1)], type.MetadataName,
            methods.ToImmutable(), properties.ToImmutable(), type.Interfaces);
        return true;
    }
}
