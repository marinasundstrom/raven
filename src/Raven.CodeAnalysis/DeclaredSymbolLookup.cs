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
        if (_semanticModel.TryGetMacroReplacementSyntax(node, out var replacementNode))
            node = replacementNode;

        if (node is TypeDeclarationSyntax generatedContainingType &&
            _semanticModel.TryGetMacroContainingTypeSyntax(generatedContainingType, out var containingTypeReplacement))
        {
            node = containingTypeReplacement;
        }

        if (TryLookupKnownDeclaredSymbolFast(node, out var fastSymbol))
            return fastSymbol;

        if (node is ParameterSyntax fastParameterSyntax &&
            TryLookupParameterSymbolFast(fastParameterSyntax, out var fastParameterSymbol))
        {
            return fastParameterSymbol;
        }

        if (!_semanticModel.DeclarationsComplete)
            _semanticModel.EnsureDeclarations();

        if (TryLookupKnownDeclaredSymbolFast(node, out fastSymbol))
            return fastSymbol;

        if (node is ParameterSyntax declaredParameterSyntax &&
            TryLookupParameterSymbolFast(declaredParameterSyntax, out var declaredParameterSymbol))
        {
            return declaredParameterSymbol;
        }

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

        if (node is CaseDeclarationSyntax caseClause &&
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

        if (node is PropertyDeclarationSyntax declaredPropertyDeclaration &&
            _semanticModel.TryGetPropertySymbol(declaredPropertyDeclaration, out var declaredProperty))
        {
            return declaredProperty;
        }

        if (node is PropertyDeclarationSyntax propertyDeclaration &&
            propertyDeclaration.Parent is TypeDeclarationSyntax propertyContainingType &&
            _semanticModel.GetDeclaredSymbol(propertyContainingType) is INamedTypeSymbol propertyOwner)
        {
            var identifierToken = propertyDeclaration.ExplicitInterfaceSpecifier is null
                ? propertyDeclaration.Identifier
                : propertyDeclaration.ExplicitInterfaceSpecifier.Identifier;

            var targetTree = propertyDeclaration.SyntaxTree;
            var targetSpan = propertyDeclaration.Span;

            var exactProperty = propertyOwner
                .GetMembers(identifierToken.ValueText)
                .OfType<IPropertySymbol>()
                .FirstOrDefault(property =>
                    property.DeclaringSyntaxReferences.Any(reference =>
                        reference.SyntaxTree == targetTree &&
                        reference.Span == targetSpan));

            if (exactProperty is not null)
                return exactProperty;

            var fallbackProperty = propertyOwner
                .GetMembers(identifierToken.ValueText)
                .OfType<IPropertySymbol>()
                .FirstOrDefault();

            if (fallbackProperty is not null)
                return fallbackProperty;
        }

        if (node is EventDeclarationSyntax eventDeclaration &&
            eventDeclaration.Parent is TypeDeclarationSyntax eventContainingType &&
            _semanticModel.GetDeclaredSymbol(eventContainingType) is INamedTypeSymbol eventOwner)
        {
            var identifierToken = eventDeclaration.ExplicitInterfaceSpecifier is null
                ? eventDeclaration.Identifier
                : eventDeclaration.ExplicitInterfaceSpecifier.Identifier;

            var targetTree = eventDeclaration.SyntaxTree;
            var targetSpan = eventDeclaration.Span;

            var exactEvent = eventOwner
                .GetMembers(identifierToken.ValueText)
                .OfType<IEventSymbol>()
                .FirstOrDefault(@event =>
                    @event.DeclaringSyntaxReferences.Any(reference =>
                        reference.SyntaxTree == targetTree &&
                        reference.Span == targetSpan));

            if (exactEvent is not null)
                return exactEvent;

            var fallbackEvent = eventOwner
                .GetMembers(identifierToken.ValueText)
                .OfType<IEventSymbol>()
                .FirstOrDefault();

            if (fallbackEvent is not null)
                return fallbackEvent;
        }

        if (node is ParameterSyntax parameterSyntax &&
            parameterSyntax.Parent?.Parent is TypeDeclarationSyntax parameterContainingType &&
            _semanticModel.GetDeclaredSymbol(parameterContainingType) is INamedTypeSymbol containingType)
        {
            var parameterSymbol = containingType
                .GetMembers(".ctor")
                .OfType<IMethodSymbol>()
                .SelectMany(method => method.Parameters)
                .FirstOrDefault(parameter => parameter.DeclaringSyntaxReferences.Any(reference =>
                    reference.SyntaxTree == parameterSyntax.SyntaxTree &&
                    reference.Span == parameterSyntax.Span));

            if (parameterSymbol is not null)
                return parameterSymbol;
        }

        if (node is ParameterSyntax lambdaParameterSyntax &&
            lambdaParameterSyntax.Ancestors().OfType<FunctionExpressionSyntax>().FirstOrDefault() is { } functionExpression)
        {
            var contextualParameterSymbol = _semanticModel.GetFunctionExpressionParameterSymbol(lambdaParameterSyntax);
            if (contextualParameterSymbol is not null)
                return contextualParameterSymbol;
        }

        if (node is VariableDeclaratorSyntax variableDeclarator &&
            _semanticModel.TryGetStableLocalDeclarationSymbol(variableDeclarator, out var stableLocalSymbol))
        {
            return stableLocalSymbol;
        }

        if (node is FunctionExpressionSyntax functionExpressionSyntax &&
            _semanticModel.TryGetFunctionExpressionSymbol(functionExpressionSyntax, out var functionExpressionSymbol))
        {
            return functionExpressionSymbol;
        }

        if (!_semanticModel.RootBinderCreated)
            _semanticModel.EnsureRootBinderCreated();

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

    private bool TryLookupParameterSymbolFast(ParameterSyntax parameterSyntax, out ISymbol? symbol)
    {
        symbol = null;

        if (parameterSyntax.Parent?.Parent is TypeDeclarationSyntax parameterContainingType &&
            TryLookupKnownDeclaredSymbolFast(parameterContainingType, out var containingTypeSymbol) &&
            containingTypeSymbol is INamedTypeSymbol containingType)
        {
            symbol = containingType
                .GetMembers(".ctor")
                .OfType<IMethodSymbol>()
                .SelectMany(method => method.Parameters)
                .FirstOrDefault(parameter => SymbolDeclarationUtilities.HasDeclaringSpan(parameter, parameterSyntax));
            return symbol is not null;
        }

        if (parameterSyntax.Parent?.Parent is MethodDeclarationSyntax methodDeclaration &&
            TryLookupKnownDeclaredSymbolFast(methodDeclaration, out var methodSymbol) &&
            methodSymbol is IMethodSymbol method)
        {
            symbol = method.Parameters.FirstOrDefault(parameter =>
                SymbolDeclarationUtilities.HasDeclaringSpan(parameter, parameterSyntax));
            return symbol is not null;
        }

        return false;
    }

    private bool TryLookupKnownDeclaredSymbolFast(SyntaxNode node, out ISymbol? symbol)
    {
        switch (node)
        {
            case CaseDeclarationSyntax caseClause when _semanticModel.TryGetUnionCaseSymbol(caseClause, out var caseSymbol):
                symbol = caseSymbol;
                return true;

            case TypeDeclarationSyntax typeDeclaration when _semanticModel.TryGetClassSymbol(typeDeclaration, out var typeSymbol):
                symbol = typeSymbol;
                return true;

            case UnionDeclarationSyntax unionDeclaration when _semanticModel.TryGetUnionSymbol(unionDeclaration, out var unionSymbol):
                symbol = unionSymbol;
                return true;

            case MethodDeclarationSyntax methodDeclaration when _semanticModel.TryGetMethodSymbol(methodDeclaration, out var methodSymbol):
                symbol = methodSymbol;
                return true;

            case PropertyDeclarationSyntax propertyDeclaration when _semanticModel.TryGetPropertySymbol(propertyDeclaration, out var propertySymbol):
                symbol = propertySymbol;
                return true;

            case EventDeclarationSyntax eventDeclaration when _semanticModel.TryGetEventSymbol(eventDeclaration, out var eventSymbol):
                symbol = eventSymbol;
                return true;

            case EventDeclarationSyntax eventDeclaration when TryLookupEventSymbolFast(eventDeclaration, out var eventSymbol):
                symbol = eventSymbol;
                return true;

            case AccessorDeclarationSyntax accessorDeclaration when TryLookupAccessorSymbolFast(accessorDeclaration, out var accessorSymbol):
                symbol = accessorSymbol;
                return true;

            default:
                symbol = null;
                return false;
        }
    }

    private bool TryLookupEventSymbolFast(
        EventDeclarationSyntax eventDeclaration,
        out IEventSymbol? eventSymbol)
    {
        eventSymbol = null;

        if (eventDeclaration.Parent is not TypeDeclarationSyntax containingTypeSyntax ||
            !TryLookupKnownDeclaredSymbolFast(containingTypeSyntax, out var containingTypeSymbol) ||
            containingTypeSymbol is not INamedTypeSymbol containingType)
        {
            return false;
        }

        var identifierToken = eventDeclaration.ExplicitInterfaceSpecifier is null
            ? eventDeclaration.Identifier
            : eventDeclaration.ExplicitInterfaceSpecifier.Identifier;

        eventSymbol = containingType
            .GetMembers(identifierToken.ValueText)
            .OfType<IEventSymbol>()
            .FirstOrDefault(@event => SymbolDeclarationUtilities.HasDeclaringSpan(@event, eventDeclaration))
            ?? containingType
                .GetMembers(identifierToken.ValueText)
                .OfType<IEventSymbol>()
                .FirstOrDefault();

        return eventSymbol is not null;
    }

    private bool TryLookupAccessorSymbolFast(
        AccessorDeclarationSyntax accessorDeclaration,
        out IMethodSymbol? accessorSymbol)
    {
        accessorSymbol = null;

        if (accessorDeclaration.Ancestors().OfType<PropertyDeclarationSyntax>().FirstOrDefault() is { } propertyDeclaration &&
            TryLookupKnownDeclaredSymbolFast(propertyDeclaration, out var propertySymbol) &&
            propertySymbol is IPropertySymbol property)
        {
            if (IsGetAccessor(accessorDeclaration))
                accessorSymbol = property.GetMethod;
            else if (IsSetAccessor(accessorDeclaration))
                accessorSymbol = property.SetMethod;

            return accessorSymbol is not null;
        }

        if (accessorDeclaration.Ancestors().OfType<EventDeclarationSyntax>().FirstOrDefault() is { } eventDeclaration &&
            TryLookupEventSymbolFast(eventDeclaration, out var eventSymbol))
        {
            if (IsAddAccessor(accessorDeclaration))
                accessorSymbol = eventSymbol.AddMethod;
            else if (IsRemoveAccessor(accessorDeclaration))
                accessorSymbol = eventSymbol.RemoveMethod;

            return accessorSymbol is not null;
        }

        return false;
    }

    private static bool IsGetAccessor(AccessorDeclarationSyntax accessor)
        => accessor.Kind == SyntaxKind.GetAccessorDeclaration || accessor.Keyword.Kind == SyntaxKind.GetKeyword;

    private static bool IsSetAccessor(AccessorDeclarationSyntax accessor)
        => accessor.Kind == SyntaxKind.SetAccessorDeclaration || accessor.Keyword.Kind == SyntaxKind.SetKeyword;

    private static bool IsAddAccessor(AccessorDeclarationSyntax accessor)
        => accessor.Kind == SyntaxKind.AddAccessorDeclaration || accessor.Keyword.Kind == SyntaxKind.AddKeyword;

    private static bool IsRemoveAccessor(AccessorDeclarationSyntax accessor)
        => accessor.Kind == SyntaxKind.RemoveAccessorDeclaration || accessor.Keyword.Kind == SyntaxKind.RemoveKeyword;

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

    private static bool TryGetLambdaParameterIndex(
        FunctionExpressionSyntax functionExpression,
        ParameterSyntax parameterSyntax,
        out int parameterIndex)
    {
        switch (functionExpression)
        {
            case ParenthesizedFunctionExpressionSyntax parenthesized:
                for (var i = 0; i < parenthesized.ParameterList.Parameters.Count; i++)
                {
                    if (ReferenceEquals(parenthesized.ParameterList.Parameters[i], parameterSyntax))
                    {
                        parameterIndex = i;
                        return true;
                    }
                }

                break;

            case SimpleFunctionExpressionSyntax simple when ReferenceEquals(simple.Parameter, parameterSyntax):
                parameterIndex = 0;
                return true;
        }

        parameterIndex = -1;
        return false;
    }
}
