using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Bodyless declarations are separate from callable body plans. The same logical
// signatures describe ordinary CLI and native interface contracts.
internal sealed record SourceInterfaceMethod(IMethodSymbol Symbol, CallableSignature Signature);
internal sealed record SourceInterfacePlan(INamedTypeSymbol Symbol, string Namespace, string Name,
    ImmutableArray<SourceInterfaceMethod> Methods)
{
    internal static bool TryCreate(INamedTypeSymbol type, EmissionCapabilities capabilities, out SourceInterfacePlan? plan)
    {
        plan = null;
        if (type.TypeKind != TypeKind.Interface || type.ContainingType is not null || !type.Interfaces.IsEmpty ||
            !capabilities.Allows(EmissionDeclarationKind.Interface) || !capabilities.Allows(EmissionDeclarationKind.InterfaceMethod) ||
            !capabilities.AllowsTypeVisibility(type.DeclaredAccessibility) || !capabilities.AllowsMethodVisibility(Accessibility.Public) ||
            type.Arity > 0 && !capabilities.AllowsGenericInterfaceDeclarations ||
            type.TypeParameters.Any(p => p.Variance != VarianceKind.None || p.ConstraintKind != TypeParameterConstraintKind.None || !p.ConstraintTypes.IsEmpty) ||
            type.DeclaringSyntaxReferences.Length != 1 || type.DeclaringSyntaxReferences[0].GetSyntax() is not InterfaceDeclarationSyntax syntax ||
            syntax.AttributeLists.Count != 0 || syntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword)))
            return false;
        if (syntax.Members.Any(m => m is not MethodDeclarationSyntax { Body: null, ExpressionBody: null } method ||
            method.AttributeLists.Count != 0 || method.ExplicitInterfaceSpecifier is not null || method.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.AbstractKeyword)))) return false;
        var methods = ImmutableArray.CreateBuilder<SourceInterfaceMethod>();
        foreach (var member in type.GetMembers())
        {
            if (member is not IMethodSymbol { IsStatic: false, IsGenericMethod: false, MethodKind: MethodKind.Ordinary, IsAbstract: true } method ||
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
        }
        var fullName = type.ToFullyQualifiedMetadataName();
        plan = new(type, type.ContainingNamespace.IsGlobalNamespace ? "" : fullName[..^(type.MetadataName.Length + 1)], type.MetadataName, methods.ToImmutable());
        return true;
    }
}
