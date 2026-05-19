using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static partial class SymbolResolver
{
    private static bool TryResolveParameterDeclarationSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var parameter = node as ParameterSyntax ?? node.AncestorsAndSelf().OfType<ParameterSyntax>().FirstOrDefault();
        if (parameter is null)
            return false;

        if (!parameter.Span.Contains(token.Span))
            return false;

        var isParameterIdentifier = parameter.Identifier.Kind == token.Kind &&
            parameter.Identifier.Span == token.Span;
        if (!isParameterIdentifier &&
            parameter.Pattern?.Span.Contains(token.Span) == true)
            return false;

        // Let nested syntax keep their own hover identity:
        // parameter type annotation should hover the type, and default values
        // should hover symbols inside the expression.
        if (parameter.TypeAnnotation?.Type.Span.Contains(token.Span) == true)
            return false;

        if (parameter.DefaultValue?.Value.Span.Contains(token.Span) == true)
            return false;

        symbol = semanticModel.GetFunctionExpressionParameterSymbol(parameter)
            ?? semanticModel.GetDeclaredSymbol(parameter);
        return symbol is not null;
    }

    private static bool TryResolveMemberReceiverSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        if (node is not IdentifierNameSyntax identifier ||
            identifier.Parent is not MemberAccessExpressionSyntax memberAccess ||
            !HaveEquivalentSpan(memberAccess.Expression, identifier) ||
            !identifier.Span.Contains(token.Span))
        {
            return false;
        }

        if (!TryGetSymbolInfo(semanticModel, identifier, out var receiverInfo))
            return false;
        var receiverCandidate = ChoosePreferredSymbol(receiverInfo.Symbol, receiverInfo.CandidateSymbols, identifier);
        if (receiverCandidate is IParameterSymbol { Type.TypeKind: not TypeKind.Error } parameter)
        {
            symbol = ProjectSymbolForDisplay(parameter);
            return symbol is not null;
        }

        if (receiverCandidate is ILocalSymbol { Type.TypeKind: not TypeKind.Error } local)
        {
            symbol = ProjectSymbolForDisplay(local);
            return symbol is not null;
        }

        return false;
    }

    private static bool TryResolveMemberSegmentSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        SyntaxNode? targetNode = node switch
        {
            MemberBindingExpressionSyntax memberBinding
                when memberBinding.Name.Span.Contains(token.Span) || memberBinding.OperatorToken == token => memberBinding,
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberBindingExpressionSyntax memberBinding &&
                     HaveEquivalentSpan(memberBinding.Name, identifier) => memberBinding,
            MemberAccessExpressionSyntax memberAccess
                when memberAccess.Name.Span.Contains(token.Span) || memberAccess.OperatorToken == token => memberAccess,
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
                     HaveEquivalentSpan(memberAccess.Name, identifier) => memberAccess,
            _ => null
        };

        if (targetNode is null)
            return false;

        if (targetNode is MemberAccessExpressionSyntax memberAccessTarget)
        {
            symbol = ResolveExplicitMemberAccessSymbol(semanticModel, memberAccessTarget, token.Span);
            if (symbol is not null)
                return true;
        }

        if (targetNode is MemberBindingExpressionSyntax memberBindingTarget)
        {
            if (TryGetSymbolInfo(semanticModel, memberBindingTarget, out var directMemberBindingInfo) &&
                (directMemberBindingInfo.Symbol is not null || !directMemberBindingInfo.CandidateSymbols.IsDefaultOrEmpty))
            {
                symbol = ChoosePreferredSymbol(directMemberBindingInfo.Symbol, directMemberBindingInfo.CandidateSymbols, memberBindingTarget);
                symbol = ProjectSymbolForDisplay(symbol);
                if (symbol is not ITypeSymbol && symbol is not null)
                    return true;
            }

            if (TryGetSymbolInfo(semanticModel, memberBindingTarget.Name, out var nameInfo) &&
                (nameInfo.Symbol is not null || !nameInfo.CandidateSymbols.IsDefaultOrEmpty))
            {
                symbol = ChoosePreferredSymbol(nameInfo.Symbol, nameInfo.CandidateSymbols, memberBindingTarget.Name);
                symbol = ProjectSymbolForDisplay(symbol);
                if (symbol is not null)
                    return true;
            }
        }

        if (TryGetSymbolInfo(semanticModel, targetNode, out var directInfo) &&
            (directInfo.Symbol is not null || !directInfo.CandidateSymbols.IsDefaultOrEmpty))
        {
            symbol = ChoosePreferredSymbol(directInfo.Symbol, directInfo.CandidateSymbols, targetNode);
            symbol = ProjectSymbolForDisplay(symbol);
            return symbol is not null;
        }

        return false;
    }

    private static bool TryResolveParameterDeclarationTokenFastPath(
        SemanticModel semanticModel,
        SyntaxToken token,
        [NotNullWhen(true)] out ISymbol? symbol,
        [NotNullWhen(true)] out SyntaxNode? node)
    {
        symbol = null;
        node = null;

        var parameter = token.Parent?.AncestorsAndSelf().OfType<ParameterSyntax>().FirstOrDefault();
        if (parameter is null)
            return false;

        if (!parameter.Span.Contains(token.Span))
            return false;

        var isParameterIdentifier = parameter.Identifier.Kind == token.Kind &&
            parameter.Identifier.Span == token.Span;
        if (!isParameterIdentifier &&
            parameter.Pattern?.Span.Contains(token.Span) == true)
            return false;

        if (parameter.TypeAnnotation?.Type.Span.Contains(token.Span) == true)
            return false;

        if (parameter.DefaultValue?.Value.Span.Contains(token.Span) == true)
            return false;

        var resolved = semanticModel.GetFunctionExpressionParameterSymbol(parameter)
            ?? semanticModel.GetDeclaredSymbol(parameter);
        if (resolved is null)
            return false;

        symbol = resolved;
        node = parameter;
        return true;
    }

    private static bool TryResolveTypePositionSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var typeSyntaxes = node.AncestorsAndSelf()
            .OfType<TypeSyntax>()
            .Where(typeSyntax => typeSyntax.Span.Contains(token.Span) &&
                                 IsExplicitTypeSyntaxContext(typeSyntax));

        foreach (var typeSyntax in typeSyntaxes)
        {
            if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, typeSyntax, out var resolvedType))
                continue;

            symbol = resolvedType!.UnderlyingUnionType ?? resolvedType;
            return true;
        }

        return false;
    }

    private static bool TryResolveTypeSymbolFromSyntax(
        SemanticModel semanticModel,
        TypeSyntax typeSyntax,
        out ITypeSymbol? resolvedType)
    {
        var typeInfo = semanticModel.TryGetAvailableTypeInfo(typeSyntax, out var availableTypeInfo)
            ? availableTypeInfo
            : semanticModel.GetTypeInfo(typeSyntax);

        resolvedType = typeInfo.Type ?? typeInfo.ConvertedType;
        if (resolvedType is not null && resolvedType.TypeKind != TypeKind.Error)
            return true;

        var typeSymbol = TryGetSymbolInfo(semanticModel, typeSyntax, out var typeSymbolInfo)
            ? typeSymbolInfo.Symbol
            : null;
        resolvedType = typeSymbol switch
        {
            ITypeSymbol resolved => resolved,
            IAliasSymbol { UnderlyingSymbol: ITypeSymbol aliasedType } => aliasedType,
            _ => resolvedType
        };

        if (resolvedType is null || resolvedType.TypeKind == TypeKind.Error)
            return false;

        return true;
    }

    private static bool TryResolveWholeTypeSymbolFromSyntax(
        SemanticModel semanticModel,
        TypeSyntax typeSyntax,
        out ITypeSymbol? resolvedType)
    {
        resolvedType = null;

        var typeInfo = semanticModel.TryGetAvailableTypeInfo(typeSyntax, out var availableTypeInfo)
            ? availableTypeInfo
            : semanticModel.GetTypeInfo(typeSyntax);
        resolvedType = typeInfo.Type ?? typeInfo.ConvertedType;
        if (resolvedType is not null && resolvedType.TypeKind != TypeKind.Error)
            return true;

        switch (typeSyntax)
        {
            case PredefinedTypeSyntax predefinedType:
                resolvedType = semanticModel.Compilation.ResolvePredefinedType(predefinedType);
                return true;

            case UnitTypeSyntax:
                resolvedType = semanticModel.Compilation.GetSpecialType(SpecialType.System_Unit);
                return true;
        }

        switch (typeSyntax)
        {
            case ArrayTypeSyntax arrayType:
                if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, arrayType.ElementType, out var elementType) ||
                    elementType is null)
                {
                    return false;
                }

                resolvedType = elementType;
                foreach (var rankSpecifier in arrayType.RankSpecifiers)
                {
                    var rank = rankSpecifier.CommaTokens.Count + 1;
                    resolvedType = semanticModel.Compilation.CreateArrayTypeSymbol(resolvedType, rank);
                }

                return true;

            case PointerTypeSyntax pointerType:
                if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, pointerType.ElementType, out var pointedAtType) ||
                    pointedAtType is null)
                {
                    return false;
                }

                resolvedType = semanticModel.Compilation.CreatePointerTypeSymbol(pointedAtType);
                return true;

            case ByRefTypeSyntax byRefType:
                if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, byRefType.ElementType, out var byRefElementType) ||
                    byRefElementType is null)
                {
                    return false;
                }

                resolvedType = new RefTypeSymbol(byRefElementType);
                return true;

            case NullableTypeSyntax nullableType:
                if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, nullableType.ElementType, out var nullableElementType) ||
                    nullableElementType is null)
                {
                    return false;
                }

                resolvedType = nullableElementType.MakeNullable();
                return true;

            case GenericNameSyntax genericName:
                if (!TryGetSymbolInfo(semanticModel, genericName, out var genericInfo))
                {
                    resolvedType = semanticModel.Compilation.ErrorTypeSymbol;
                    return true;
                }

                var namedType = genericInfo.Symbol as INamedTypeSymbol
                    ?? (!genericInfo.CandidateSymbols.IsDefaultOrEmpty
                        ? genericInfo.CandidateSymbols.OfType<INamedTypeSymbol>().FirstOrDefault()
                        : null);
                if (namedType is null)
                {
                    resolvedType = semanticModel.Compilation.ErrorTypeSymbol;
                    return true;
                }

                if (namedType.TypeKind == TypeKind.Error)
                {
                    resolvedType = namedType;
                    return true;
                }

                var typeArguments = new List<ITypeSymbol>(genericName.TypeArgumentList.Arguments.Count);
                foreach (var argument in genericName.TypeArgumentList.Arguments)
                {
                    if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, argument.Type, out var typeArgument) ||
                        typeArgument is null)
                    {
                        return false;
                    }

                    typeArguments.Add(typeArgument);
                }

                if (namedType.TypeArguments.Length == typeArguments.Count &&
                    namedType.TypeArguments.Zip(typeArguments, SymbolEqualityComparer.Default.Equals).All(static equals => equals))
                {
                    resolvedType = namedType;
                    return true;
                }

                var definition = namedType.ConstructedFrom as INamedTypeSymbol ?? namedType;
                resolvedType = definition.Construct(typeArguments.ToArray()) as ITypeSymbol;
                return resolvedType is not null;

            case TupleTypeSyntax tupleType:
                var elements = new List<(string? name, ITypeSymbol type)>(tupleType.Elements.Count);
                foreach (var element in tupleType.Elements)
                {
                    if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, element.Type, out var tupleElementType) ||
                        tupleElementType is null)
                    {
                        return false;
                    }

                    elements.Add((element.NameColon?.Name.ToString(), tupleElementType));
                }

                resolvedType = semanticModel.Compilation.CreateTupleTypeSymbol(elements);
                if (resolvedType.TypeKind == TypeKind.Error)
                    resolvedType = semanticModel.Compilation.ErrorTypeSymbol;

                return true;

            case FunctionTypeSyntax functionType:
                var parameterTypes = new List<ITypeSymbol>();

                if (functionType.ParameterList is not null)
                {
                    foreach (var parameter in functionType.ParameterList.Parameters)
                    {
                        if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, parameter, out var parameterType) ||
                            parameterType is null)
                        {
                            return false;
                        }

                        parameterTypes.Add(parameterType);
                    }
                }
                else if (functionType.Parameter is not null)
                {
                    if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, functionType.Parameter, out var parameterType) ||
                        parameterType is null)
                    {
                        return false;
                    }

                    parameterTypes.Add(parameterType);
                }

                if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, functionType.ReturnType, out var returnType) ||
                    returnType is null)
                {
                    return false;
                }

                resolvedType = semanticModel.Compilation.CreateFunctionTypeSymbol(parameterTypes.ToArray(), returnType);
                return true;

            case UnionTypeSyntax unionType:
                var unionMembers = new List<ITypeSymbol>(unionType.Types.Count);
                foreach (var member in unionType.Types)
                {
                    if (!TryResolveWholeTypeSymbolFromSyntax(semanticModel, member, out var memberType) ||
                        memberType is null)
                    {
                        return false;
                    }

                    unionMembers.Add(memberType);
                }

                var unionDefinition = semanticModel.Compilation.GetTypeByMetadataName($"System.Union`{unionMembers.Count}") as INamedTypeSymbol;
                resolvedType = unionDefinition?.Construct(unionMembers.ToArray()) as ITypeSymbol
                    ?? semanticModel.Compilation.ErrorTypeSymbol;
                return true;
        }

        if (TryResolveTypeSymbolFromSyntax(semanticModel, typeSyntax, out resolvedType) &&
            resolvedType is not null &&
            resolvedType.TypeKind != TypeKind.Error)
        {
            return true;
        }

        typeInfo = semanticModel.GetTypeInfo(typeSyntax);
        resolvedType = typeInfo.Type ?? typeInfo.ConvertedType;
        if (resolvedType is not null)
            return true;

        return false;
    }

    private static ISymbol? ResolveExplicitMemberAccessSymbol(
        SemanticModel semanticModel,
        MemberAccessExpressionSyntax memberAccess,
        TextSpan tokenSpan)
    {
        if (memberAccess.Parent is InvocationExpressionSyntax invocation &&
            memberAccess.Name.Span.Contains(tokenSpan.Start) &&
            TryResolveUnionCaseFromInvocationContext(semanticModel, invocation, out var unionCaseSymbol) &&
            unionCaseSymbol is not null)
        {
            return unionCaseSymbol;
        }

        if (memberAccess.Name.Span.Contains(tokenSpan) &&
            TryGetSymbolInfo(semanticModel, memberAccess, out var cachedMemberAccessInfo))
        {
            var chosenCachedMemberAccessSymbol = ProjectSymbolForDisplay(
                ChoosePreferredSymbol(cachedMemberAccessInfo.Symbol, cachedMemberAccessInfo.CandidateSymbols, memberAccess));
            if (chosenCachedMemberAccessSymbol is not null)
                return chosenCachedMemberAccessSymbol;
        }

        if (memberAccess.Name.Span.Contains(tokenSpan) &&
            TryGetSymbolInfo(semanticModel, memberAccess.Name, out var cachedNameInfo))
        {
            var chosenCachedNameSymbol = ProjectSymbolForDisplay(
                ChoosePreferredSymbol(cachedNameInfo.Symbol, cachedNameInfo.CandidateSymbols, memberAccess.Name));
            if (chosenCachedNameSymbol is not null)
                return chosenCachedNameSymbol;
        }

        if (TryGetSymbolInfo(semanticModel, memberAccess, out var memberAccessInfo))
        {
            var chosenMemberAccessSymbol = ProjectSymbolForDisplay(
                ChoosePreferredSymbol(memberAccessInfo.Symbol, memberAccessInfo.CandidateSymbols, memberAccess));
            if (chosenMemberAccessSymbol is not null)
                return chosenMemberAccessSymbol;
        }

        if (TryGetSymbolInfo(semanticModel, memberAccess.Name, out var nameInfo))
        {
            var chosenNameSymbol = ProjectSymbolForDisplay(
                ChoosePreferredSymbol(nameInfo.Symbol, nameInfo.CandidateSymbols, memberAccess.Name));
            if (chosenNameSymbol is not null)
                return chosenNameSymbol;
        }

        return null;
    }

}
