using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class MemberCanBeStaticAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9017";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Member can be static",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Method '{0}' does not access instance data and can be static.",
        category: "Design",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzeMethod, SyntaxKind.MethodDeclaration);
    }

    private static void AnalyzeMethod(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not MethodDeclarationSyntax methodDecl)
            return;

        if (context.SemanticModel.GetDeclaredSymbol(methodDecl) is not IMethodSymbol methodSymbol)
            return;

        if (!CanBeStatic(methodDecl, methodSymbol))
            return;

        if (UsesInstanceState(methodDecl, methodSymbol, context.SemanticModel))
            return;

        context.ReportDiagnostic(Diagnostic.Create(
            Descriptor,
            methodDecl.Identifier.GetLocation(),
            methodSymbol.Name));
    }

    private static bool CanBeStatic(MethodDeclarationSyntax methodDecl, IMethodSymbol methodSymbol)
    {
        if (methodSymbol.IsStatic ||
            methodSymbol.IsAbstract ||
            methodSymbol.IsVirtual ||
            methodSymbol.IsOverride ||
            methodSymbol.IsExtern ||
            methodSymbol.MethodKind != MethodKind.Ordinary ||
            !methodSymbol.ExplicitInterfaceImplementations.IsDefaultOrEmpty ||
            methodSymbol.ContainingType is null ||
            methodSymbol.ContainingType.TypeKind is not (TypeKind.Class or TypeKind.Struct) ||
            methodSymbol.ContainingType.IsStatic ||
            ImplementsInterfaceMember(methodSymbol))
        {
            return false;
        }

        return methodDecl.Body is not null || methodDecl.ExpressionBody is not null;
    }

    private static bool ImplementsInterfaceMember(IMethodSymbol methodSymbol)
    {
        if (methodSymbol.ContainingType is null)
            return false;

        foreach (var interfaceType in methodSymbol.ContainingType.AllInterfaces)
        {
            foreach (var interfaceMember in interfaceType.GetMembers(methodSymbol.Name))
            {
                if (interfaceMember is IMethodSymbol interfaceMethod &&
                    IsMatchingInterfaceMethod(methodSymbol, interfaceMethod))
                {
                    return true;
                }
            }
        }

        return false;
    }

    private static bool IsMatchingInterfaceMethod(IMethodSymbol methodSymbol, IMethodSymbol interfaceMethod)
    {
        return methodSymbol.MethodKind == MethodKind.Ordinary &&
            interfaceMethod.MethodKind == MethodKind.Ordinary &&
            HasSameParameters(methodSymbol.Parameters, interfaceMethod.Parameters) &&
            SymbolEqualityComparer.Default.Equals(methodSymbol.ReturnType, interfaceMethod.ReturnType);
    }

    private static bool HasSameParameters(
        ImmutableArray<IParameterSymbol> left,
        ImmutableArray<IParameterSymbol> right)
    {
        if (left.Length != right.Length)
            return false;

        for (var i = 0; i < left.Length; i++)
        {
            if (left[i].RefKind != right[i].RefKind)
                return false;

            if (!SymbolEqualityComparer.Default.Equals(left[i].Type, right[i].Type))
                return false;
        }

        return true;
    }

    private static bool UsesInstanceState(
        MethodDeclarationSyntax methodDecl,
        IMethodSymbol methodSymbol,
        SemanticModel semanticModel)
    {
        SyntaxNode? body = methodDecl.Body is not null
            ? methodDecl.Body
            : methodDecl.ExpressionBody?.Expression;
        if (body is null)
            return false;

        foreach (var node in body.DescendantNodesAndSelf())
        {
            if (node is SelfExpressionSyntax)
                return true;

            if (node is not (IdentifierNameSyntax or MemberAccessExpressionSyntax or InvocationExpressionSyntax))
                continue;

            var referenced = ResolveReferencedSymbol(node, semanticModel);
            if (referenced is null || referenced.IsStatic)
                continue;

            if (referenced is IParameterSymbol parameterSymbol &&
                IsPrimaryConstructorParameterOfContainingType(parameterSymbol, methodSymbol.ContainingType))
            {
                return true;
            }

            if (referenced.ContainingType is null ||
                !SymbolEqualityComparer.Default.Equals(referenced.ContainingType, methodSymbol.ContainingType))
            {
                continue;
            }

            if (referenced.Kind is not (SymbolKind.Field or SymbolKind.Property or SymbolKind.Event or SymbolKind.Method))
                continue;

            if (SymbolEqualityComparer.Default.Equals(referenced, methodSymbol))
                continue;

            return true;
        }

        return false;
    }

    private static ISymbol? ResolveReferencedSymbol(SyntaxNode node, SemanticModel semanticModel)
    {
        if (node is IdentifierNameSyntax identifier)
        {
            if (semanticModel.TryLookupVisibleValueSymbol(identifier) is { } visibleSymbol)
                return visibleSymbol.UnderlyingSymbol;
        }

        return semanticModel.GetSymbolInfo(node).Symbol?.UnderlyingSymbol;
    }

    private static bool IsPrimaryConstructorParameterOfContainingType(
        IParameterSymbol parameterSymbol,
        INamedTypeSymbol? containingType)
    {
        if (containingType is null)
            return false;

        if (parameterSymbol.ContainingSymbol is not IMethodSymbol methodSymbol ||
            methodSymbol.MethodKind != MethodKind.Constructor)
        {
            return false;
        }

        if (!SymbolEqualityComparer.Default.Equals(methodSymbol.ContainingType, containingType))
            return false;

        foreach (var syntaxReference in methodSymbol.DeclaringSyntaxReferences)
        {
            if (syntaxReference.GetSyntax() is TypeDeclarationSyntax { ParameterList: not null })
                return true;
        }

        return false;
    }
}
