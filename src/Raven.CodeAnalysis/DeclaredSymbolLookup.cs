using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class DeclaredSymbolLookup
{
    private readonly SemanticModel _semanticModel;

    public DeclaredSymbolLookup(SemanticModel semanticModel)
    {
        _semanticModel = semanticModel;
    }

    public ISymbol? Lookup(SyntaxNode node)
    {
        _semanticModel.EnsureDeclarations();
        _semanticModel.EnsureRootBinderCreated();

        if (node is MethodDeclarationSyntax methodDeclarationByContainingType &&
            methodDeclarationByContainingType.Parent is TypeDeclarationSyntax containingTypeSyntax &&
            LookupMethodByContainingType(methodDeclarationByContainingType, containingTypeSyntax) is { } methodFromContainingType)
        {
            _semanticModel.EnsureAsyncLoweredForDeclaredMethod(methodDeclarationByContainingType, methodFromContainingType);
            return methodFromContainingType;
        }

        if (node is MethodDeclarationSyntax methodSyntaxFromBody)
        {
            if (methodSyntaxFromBody.Body is not null &&
                _semanticModel.GetBinder(methodSyntaxFromBody.Body).ContainingSymbol is IMethodSymbol methodFromBodyBinder)
            {
                _semanticModel.EnsureAsyncLoweredForDeclaredMethod(methodSyntaxFromBody, methodFromBodyBinder);
                return methodFromBodyBinder;
            }

            if (methodSyntaxFromBody.ExpressionBody is not null &&
                _semanticModel.GetBinder(methodSyntaxFromBody.ExpressionBody.Expression).ContainingSymbol is IMethodSymbol methodFromExpressionBodyBinder)
            {
                _semanticModel.EnsureAsyncLoweredForDeclaredMethod(methodSyntaxFromBody, methodFromExpressionBodyBinder);
                return methodFromExpressionBodyBinder;
            }
        }

        if (node is UnionCaseClauseSyntax caseClause &&
            _semanticModel.TryGetUnionCaseSymbol(caseClause, out var caseSymbol))
        {
            return caseSymbol;
        }

        if (node is TypeDeclarationSyntax typeDeclaration &&
            _semanticModel.TryGetClassSymbol(typeDeclaration, out var typeSymbol))
        {
            return typeSymbol;
        }

        if (node is UnionDeclarationSyntax unionDeclaration &&
            _semanticModel.TryGetUnionSymbol(unionDeclaration, out var unionSymbol))
        {
            return unionSymbol;
        }

        if (node is MethodDeclarationSyntax methodDeclaration &&
            _semanticModel.TryGetMethodSymbol(methodDeclaration, out var declaredMethod))
        {
            _semanticModel.EnsureAsyncLoweredForDeclaredMethod(methodDeclaration, declaredMethod);
            return declaredMethod;
        }

        var binder = _semanticModel.GetBinder(node);

        if (_semanticModel.Compilation.DeclarationTable.TryGetDeclKey(node, out var key))
        {
            var declared = _semanticModel.Compilation.SymbolFactory.GetOrCreate(key, () =>
            {
                return (Symbol)binder.BindDeclaredSymbol(node)!;
            });

            if (node is MethodDeclarationSyntax methodDeclarationFromKey &&
                declared is IMethodSymbol methodSymbol)
            {
                _semanticModel.EnsureAsyncLoweredForDeclaredMethod(methodDeclarationFromKey, methodSymbol);
            }

            return declared;
        }

        return binder.BindDeclaredSymbol(node);
    }

    private IMethodSymbol? LookupMethodByContainingType(
        MethodDeclarationSyntax methodDeclaration,
        TypeDeclarationSyntax containingTypeSyntax)
    {
        if (_semanticModel.GetDeclaredSymbol(containingTypeSyntax) is not INamedTypeSymbol containingType)
            return null;

        var targetSpan = methodDeclaration.Span;
        var targetTree = methodDeclaration.SyntaxTree;
        var parameterCount = methodDeclaration.ParameterList?.Parameters.Count ?? 0;
        var arity = methodDeclaration.TypeParameterList?.Parameters.Count ?? 0;

        var exact = containingType
            .GetMembers(methodDeclaration.Identifier.ValueText)
            .OfType<IMethodSymbol>()
            .FirstOrDefault(method =>
                method.Parameters.Length == parameterCount &&
                method.Arity == arity &&
                method.DeclaringSyntaxReferences.Any(reference =>
                    reference.SyntaxTree == targetTree &&
                    reference.Span == targetSpan));

        if (exact is not null)
            return exact;

        return containingType
            .GetMembers(methodDeclaration.Identifier.ValueText)
            .OfType<IMethodSymbol>()
            .FirstOrDefault(method =>
                method.Parameters.Length == parameterCount &&
                method.Arity == arity);
    }
}
