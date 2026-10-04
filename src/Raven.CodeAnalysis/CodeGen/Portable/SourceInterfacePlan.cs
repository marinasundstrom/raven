using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Bodyless declarations are separate from callable body plans. The same logical
// signatures describe ordinary CLI and native interface contracts.
internal sealed record SourceInterfaceMethod(IMethodSymbol Symbol, CallableSignature Signature);
internal sealed record SourceInterfaceProperty(IPropertySymbol Symbol, EmissionType Type);
internal sealed record SourceInterfacePlan(INamedTypeSymbol Symbol, string Namespace, string Name,
    ImmutableArray<SourceInterfaceMethod> Methods, ImmutableArray<SourceInterfaceProperty> Properties,
    ImmutableArray<INamedTypeSymbol> BaseInterfaces)
{
    // Identity admission must not recursively inspect members: interface signatures
    // can refer back to their own definition or to another interface.
    internal static bool HasSupportedIdentity(INamedTypeSymbol type) =>
        type.TypeKind == TypeKind.Interface && type.ContainingType is null &&
        type.DeclaredAccessibility is Accessibility.Public or Accessibility.Internal &&
        type.DeclaringSyntaxReferences.Length == 1 && type.DeclaringSyntaxReferences[0].GetSyntax() is InterfaceDeclarationSyntax &&
        ((INamedTypeSymbol)type.OriginalDefinition).TypeParameters.All(p => p.Variance == VarianceKind.None &&
            p.ConstraintKind == TypeParameterConstraintKind.None && p.ConstraintTypes.IsEmpty);

    internal static bool HasSupportedRelationship(INamedTypeSymbol target, IAssemblySymbol owner, EmissionCapabilities? capabilities)
    {
        if (SymbolEqualityComparer.Default.Equals(target.ContainingAssembly, owner)) return HasSupportedIdentity(target);
        return capabilities?.AllowsExternalInterfaceDeclarations == true && target.TypeKind == TypeKind.Interface &&
            target.ContainingType is null && target.DeclaredAccessibility == Accessibility.Public &&
            ((INamedTypeSymbol)target.OriginalDefinition).TypeParameters.All(p => p.Variance == VarianceKind.None &&
                p.ConstraintKind == TypeParameterConstraintKind.None && p.ConstraintTypes.IsEmpty);
    }

    internal static bool TryCreate(INamedTypeSymbol type, EmissionCapabilities capabilities, out SourceInterfacePlan? plan)
    {
        plan = null;
        if (type.TypeKind != TypeKind.Interface || type.ContainingType is not null ||
            !capabilities.Allows(EmissionDeclarationKind.Interface) || !capabilities.Allows(EmissionDeclarationKind.InterfaceMethod) ||
            !capabilities.AllowsTypeVisibility(type.DeclaredAccessibility) || !capabilities.AllowsMethodVisibility(Accessibility.Public) ||
            type.Arity > 0 && !capabilities.AllowsGenericInterfaceDeclarations ||
            type.TypeParameters.Any(p => p.Variance != VarianceKind.None || p.ConstraintKind != TypeParameterConstraintKind.None || !p.ConstraintTypes.IsEmpty) ||
            type.DeclaringSyntaxReferences.Length != 1 || type.DeclaringSyntaxReferences[0].GetSyntax() is not InterfaceDeclarationSyntax syntax ||
            syntax.AttributeLists.Count != 0 || syntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword) &&
                !(m.Kind == SyntaxKind.SealedKeyword && capabilities.AllowsClosedInterfaceFamilies && type.Arity == 0)))
            return false;
        if (!type.Interfaces.IsEmpty && (!capabilities.Allows(EmissionDeclarationKind.InterfaceInheritance) ||
            type.Interfaces.Any(b => !HasSupportedRelationship(b, type.ContainingAssembly, capabilities) || b.Arity != 0 && (!capabilities.AllowsConstructedInterfaceInheritance ||
                !CallableSignature.TryType(b, false, out var inherited, capabilities) || !capabilities.Allows(inherited))))) return false;
        bool AllowsModifier(SyntaxToken modifier) => modifier.Kind is SyntaxKind.PublicKeyword or SyntaxKind.AbstractKeyword ||
            modifier.Kind == SyntaxKind.StaticKeyword && capabilities.Allows(EmissionDeclarationKind.StaticInterfaceMethod);
        foreach (var member in syntax.Members)
        {
            if (member is MethodDeclarationSyntax { Body: null, ExpressionBody: null } method && method.AttributeLists.Count == 0 &&
                method.ExplicitInterfaceSpecifier is null && method.Modifiers.All(AllowsModifier)) continue;
            if (member is PropertyDeclarationSyntax { ExpressionBody: null, Initializer: null, AccessorList: { } accessors } property &&
                property.AttributeLists.Count == 0 && property.ExplicitInterfaceSpecifier is null &&
                property.Modifiers.All(AllowsModifier) &&
                accessors.Accessors.All(a => a.Kind is SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration &&
                    a.Body is null && a.ExpressionBody is null && a.AttributeLists.Count == 0 && a.Modifiers.Count == 0)) continue;
            if (member is IndexerDeclarationSyntax { ExpressionBody: null, AccessorList: { } indexAccessors } indexer &&
                capabilities.Allows(EmissionDeclarationKind.InterfaceIndexer) && indexer.AttributeLists.Count == 0 && indexer.ExplicitInterfaceSpecifier is null &&
                indexer.Modifiers.All(m => m.Kind is SyntaxKind.PublicKeyword or SyntaxKind.AbstractKeyword) &&
                indexAccessors.Accessors.All(a => a.Kind is SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration &&
                    a.Body is null && a.ExpressionBody is null && a.AttributeLists.Count == 0 && a.Modifiers.Count == 0)) continue;
            if (member is OperatorDeclarationSyntax { Body: null, ExpressionBody: null } op &&
                capabilities.Allows(EmissionDeclarationKind.StaticInterfaceMethod) && op.AttributeLists.Count == 0 &&
                op.Modifiers.All(AllowsModifier)) continue;
            return false;
        }
        var methods = ImmutableArray.CreateBuilder<SourceInterfaceMethod>();
        var properties = ImmutableArray.CreateBuilder<SourceInterfaceProperty>();
        var seen = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        bool AddMethod(IMethodSymbol method)
        {
            if (!seen.Add(method)) return true;
            if (method.IsStatic && !capabilities.Allows(EmissionDeclarationKind.StaticInterfaceMethod) || method.IsGenericMethod || !method.IsAbstract ||
                method.MethodKind is not (MethodKind.Ordinary or MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.UserDefinedOperator) ||
                method.DeclaredAccessibility != Accessibility.Public || !CallableSignature.TryType(method.ReturnType, true, out var result, capabilities)) return false;
            var parameters = ImmutableArray.CreateBuilder<EmissionType>();
            foreach (var parameter in method.Parameters)
            {
                if (parameter.RefKind != RefKind.None && (!capabilities.AllowsManagedReferences || parameter.RefKind is not (RefKind.Ref or RefKind.Out)) || parameter.HasExplicitDefaultValue || !CallableSignature.AllowsParameterArray(method, parameter, capabilities) ||
                    !CallableSignature.TryType(parameter.Type, false, out var value, capabilities) || !capabilities.Allows(value)) return false;
                parameters.Add(value with { IsByReference = parameter.RefKind != RefKind.None });
            }
            if (!capabilities.Allows(result)) return false;
            methods.Add(new(method, new(result, parameters.ToImmutable(), IsInstance: !method.IsStatic, DeclaringTypeArity: type.Arity,
                OutParameters: method.Parameters.Select((p, i) => (p, i)).Where(x => x.p.RefKind == RefKind.Out).Select(x => x.i).ToImmutableArray())));
            return true;
        }
        foreach (var member in type.GetMembers())
        {
            if (member is IMethodSymbol method) { if (!AddMethod(method)) return false; }
            else if (member is IPropertySymbol property && (!property.IsStatic || capabilities.Allows(EmissionDeclarationKind.StaticInterfaceMethod)) &&
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
