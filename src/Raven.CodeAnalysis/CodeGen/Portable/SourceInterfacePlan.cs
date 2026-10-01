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
    internal static bool TryCreate(INamedTypeSymbol type, EmissionCapabilities capabilities, out SourceInterfacePlan? plan)
    {
        plan = null;
        if (type.TypeKind != TypeKind.Interface || type.ContainingType is not null ||
            !capabilities.Allows(EmissionDeclarationKind.Interface) || !capabilities.Allows(EmissionDeclarationKind.InterfaceMethod) ||
            !capabilities.AllowsTypeVisibility(type.DeclaredAccessibility) || !capabilities.AllowsMethodVisibility(Accessibility.Public) ||
            type.Arity > 0 && !capabilities.AllowsGenericInterfaceDeclarations ||
            type.TypeParameters.Any(p => p.Variance != VarianceKind.None || p.ConstraintKind != TypeParameterConstraintKind.None || !p.ConstraintTypes.IsEmpty) ||
            type.DeclaringSyntaxReferences.Length != 1 || type.DeclaringSyntaxReferences[0].GetSyntax() is not InterfaceDeclarationSyntax syntax ||
            syntax.AttributeLists.Count != 0 || syntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword)))
            return false;
        if (!type.Interfaces.IsEmpty && (!capabilities.Allows(EmissionDeclarationKind.InterfaceInheritance) ||
            type.Interfaces.Any(b => b.Arity != 0 || b.TypeKind != TypeKind.Interface || b.DeclaringSyntaxReferences.IsEmpty ||
                !SymbolEqualityComparer.Default.Equals(b.ContainingAssembly, type.ContainingAssembly)))) return false;
        foreach (var member in syntax.Members)
        {
            if (member is MethodDeclarationSyntax { Body: null, ExpressionBody: null } method && method.AttributeLists.Count == 0 &&
                method.ExplicitInterfaceSpecifier is null && method.Modifiers.All(m => m.Kind is SyntaxKind.PublicKeyword or SyntaxKind.AbstractKeyword)) continue;
            if (member is PropertyDeclarationSyntax { ExpressionBody: null, Initializer: null, AccessorList: { } accessors } property &&
                property.AttributeLists.Count == 0 && property.ExplicitInterfaceSpecifier is null &&
                property.Modifiers.All(m => m.Kind is SyntaxKind.PublicKeyword or SyntaxKind.AbstractKeyword) &&
                accessors.Accessors.All(a => a.Kind is SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration &&
                    a.Body is null && a.ExpressionBody is null && a.AttributeLists.Count == 0 && a.Modifiers.Count == 0)) continue;
            return false;
        }
        var methods = ImmutableArray.CreateBuilder<SourceInterfaceMethod>();
        var properties = ImmutableArray.CreateBuilder<SourceInterfaceProperty>();
        var seen = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        bool AddMethod(IMethodSymbol method)
        {
            if (!seen.Add(method)) return true;
            if (method.IsStatic || method.IsGenericMethod || !method.IsAbstract ||
                method.MethodKind is not (MethodKind.Ordinary or MethodKind.PropertyGet or MethodKind.PropertySet) ||
                method.DeclaredAccessibility != Accessibility.Public || !CallableSignature.TryType(method.ReturnType, true, out var result)) return false;
            var parameters = ImmutableArray.CreateBuilder<EmissionType>();
            foreach (var parameter in method.Parameters)
            {
                if (parameter.RefKind != RefKind.None || parameter.HasExplicitDefaultValue || parameter.IsVarParams ||
                    !CallableSignature.TryType(parameter.Type, false, out var value) || !capabilities.Allows(value)) return false;
                parameters.Add(value);
            }
            if (!capabilities.Allows(result)) return false;
            methods.Add(new(method, new(result, parameters.ToImmutable(), IsInstance: true, DeclaringTypeArity: type.Arity)));
            return true;
        }
        foreach (var member in type.GetMembers())
        {
            if (member is IMethodSymbol method) { if (!AddMethod(method)) return false; }
            else if (member is IPropertySymbol { IsStatic: false, IsIndexer: false } property &&
                capabilities.Allows(EmissionDeclarationKind.InterfaceProperty) && property.DeclaredAccessibility == Accessibility.Public &&
                CallableSignature.TryType(property.Type, false, out var value) && capabilities.Allows(value))
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
