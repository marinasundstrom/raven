using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private void AnalyzeStatement(ISymbol declaringSymbol, StatementSyntax statement)
    {
        switch (statement)
        {
            case LocalDeclarationStatementSyntax localDeclarationStatement:
                AnalyzeDeclarationStatement(declaringSymbol, localDeclarationStatement);
                break;

            case BlockSyntax block:
                AnalyzeBlock(declaringSymbol, block);
                break;

            case IfStatementSyntax ifStatement:
                AnalyzeIfStatement(declaringSymbol, ifStatement);
                break;

            case ReturnStatementSyntax returnStatement:
                AnalyzeReturnStatement(declaringSymbol, returnStatement);
                break;

            case ExpressionStatementSyntax expressionStatement:
                AnalyzeExpression(declaringSymbol, declaringSymbol, expressionStatement.Expression, out var s);
                break;
        }
    }

    private void AnalyzeDeclarationStatement(ISymbol declaringSymbol, LocalDeclarationStatementSyntax localDeclarationStatement)
    {
        foreach (var declarator in localDeclarationStatement.Declaration.Declarators)
        {
            Location[] locations = [SyntaxTree.GetLocation(declarator.Span)];

            SyntaxReference[] references = [new SyntaxReference(SyntaxTree, declarator.Span)];
            var typeExpr = declarator?.TypeAnnotation?.Type;

            ImmutableArray<ISymbol> expSymbols = [];

            if (declarator?.Initializer?.Value is not null)
            {
                AnalyzeExpression(declaringSymbol, declaringSymbol, declarator.Initializer.Value, out expSymbols);

                if (!expSymbols.Any())
                    return;
            }

            ITypeSymbol? propertyType;
            if (typeExpr is not null)
            {
                if (typeExpr is PredefinedTypeSyntax pdt)
                {
                    propertyType = _keywordTypeSymbols[pdt.Keyword.ToString()];
                }
                else
                {
                    propertyType = (ResolveType(typeExpr) as ITypeSymbol)!;

                    if (propertyType is null)
                    {
                        propertyType = new ErrorTypeSymbol(typeExpr.ToString(), declaringSymbol, [typeExpr.GetLocation()], [new SyntaxReference(SyntaxTree, typeExpr.Span)]); // Unable to resolve

                        Bind(typeExpr, propertyType);

                        // TODO: Centralize
                        Diagnostics.Add(
                            Diagnostic.Create(
                                CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext,
                                typeExpr.GetLocation(),
                                [typeExpr.ToString()]
                            ));
                    }
                }
            }
            else
            {
                // INFO: Might be a hack
                propertyType = expSymbols.FirstOrDefault()?.UnwrapType();
            }

            if (declarator?.Initializer?.Value is not null)
            {
                if (declarator.TypeAnnotation is null)
                {
                    var typeSymbol = expSymbols.First().UnwrapType();

                    if (typeSymbol.SpecialType == SpecialType.System_Void)
                    {
                        // TODO: Centralize
                        Diagnostics.Add(
                            Diagnostic.Create(
                                CompilerDiagnostics.CannotAssignVoidToAnImplicitlyTypedVariable,
                                declarator.Initializer.Value.GetLocation()
                            ));
                    }
                    else
                    {
                        //var z = Compilation.ClassifyConversion(typeSymbol, propertyType);
                    }
                }
            }

            var symbol = new SourceLocalSymbol(
                declarator.Name.Identifier.Text.ToString(), propertyType, declaringSymbol!, declaringSymbol.ContainingType, declaringSymbol.ContainingNamespace,
                locations, references);

            Bind(declarator, symbol);
            _localSymbols.Add(symbol);
        }
    }

    private void AnalyzeBlock(ISymbol declaringSymbol, BlockSyntax block)
    {
        foreach (var s in block.Statements)
        {
            AnalyzeStatement(declaringSymbol, s);
        }
    }

    private void AnalyzeIfStatement(ISymbol declaringSymbol, IfStatementSyntax ifStatement)
    {
        AnalyzeExpression(declaringSymbol, declaringSymbol, ifStatement.Condition, out var s);

        AnalyzeStatement(declaringSymbol, ifStatement.Statement);

        if (ifStatement.ElseClause is not null)
        {
            AnalyzeStatement(declaringSymbol, ifStatement.ElseClause.Statement);
        }
    }

    private void AnalyzeReturnStatement(ISymbol declaringSymbol, ReturnStatementSyntax returnStatement)
    {
        if (returnStatement.Expression is not null)
        {
            AnalyzeExpression(declaringSymbol, declaringSymbol, returnStatement.Expression, out var s);
        }
    }

    private ISymbol? ResolveType(TypeSyntax typeExpr)
    {
        ISymbol? innerSymbol = null;

        if (typeExpr is QualifiedNameSyntax qualifiedName)
        {
            // Resolve the left part of the qualified name
            innerSymbol = ResolveType(qualifiedName.Left);

            if (innerSymbol is INamespaceOrTypeSymbol containerSymbol)
            {
                var rightName = qualifiedName.Right.Identifier.ValueText;

                // Check if the right name exists in the inner symbol's members
                var memberSymbol = containerSymbol.GetMembers(rightName).FirstOrDefault();
                if (memberSymbol != null)
                {
                    Bind(qualifiedName, memberSymbol);
                    return memberSymbol;
                }
            }

            return null; // Unable to resolve
        }

        if (typeExpr is IdentifierNameSyntax identifierName)
        {
            // Check global namespace and imports for the symbol
            var name = identifierName.Identifier.ValueText;

            // Look in the global namespace
            var globalSymbol = Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
            if (globalSymbol != null)
            {
                Bind(typeExpr, globalSymbol);
                return globalSymbol;
            }

            // Look in imports (_imports is assumed to be a collection of namespaces)
            foreach (var (syntax, import) in _imports)
            {
                var importedSymbol = import.GetMembers(name).FirstOrDefault();
                if (importedSymbol != null)
                {
                    Bind(typeExpr, importedSymbol);
                    return importedSymbol;
                }
            }

            return null; // Symbol not found
        }

        // Handle other TypeSyntax cases as needed
        return null;
    }

    private void AnalyzeExpression(ISymbol declaringSymbol, ISymbol containingSymbol, ExpressionSyntax expression, out ImmutableArray<ISymbol> symbols)
    {
        switch (expression)
        {
            case MemberAccessExpressionSyntax memberAccessExpression:
                symbols = AnalyzeMemberAccessExpression(declaringSymbol, containingSymbol, expression, memberAccessExpression);
                break;

            case InvocationExpressionSyntax invocationExpression:
                symbols = AnalyzeInvocationExpression(declaringSymbol, containingSymbol, expression, invocationExpression);
                break;

            case PredefinedTypeSyntax predefinedTypeSyntax:
                symbols = AnalyzePredefinedType(predefinedTypeSyntax);
                break;

            case IdentifierNameSyntax name:
                symbols = AnalyzeIdentifierName(declaringSymbol, containingSymbol, expression, name);
                break;

            case LiteralExpressionSyntax literalExpression:
                symbols = AnalyzeLiteralExpression(literalExpression);
                break;

            case BinaryExpressionSyntax binaryExpression:
                symbols = AnalyzeBinaryExpression(declaringSymbol, containingSymbol, binaryExpression);
                break;

            default:
                symbols = ImmutableArray<ISymbol>.Empty;
                break;
        }
    }

    private ImmutableArray<ISymbol> AnalyzeMemberAccessExpression(ISymbol declaringSymbol, ISymbol containingSymbol, ExpressionSyntax expression, MemberAccessExpressionSyntax memberAccessExpression)
    {
        AnalyzeExpression(declaringSymbol, containingSymbol, memberAccessExpression.Expression, out var baseSymbols);

        var name = memberAccessExpression.Name.Identifier.Text;

        var resolvedSymbols = ImmutableArray.CreateBuilder<ISymbol>();

        foreach (var baseSymbol in baseSymbols)
        {
            switch (baseSymbol)
            {
                case INamespaceSymbol namespaceSymbol:
                    resolvedSymbols.AddRange(namespaceSymbol.GetMembers(name));
                    break;

                case ITypeSymbol typeSymbol:
                    resolvedSymbols.AddRange(typeSymbol.GetMembers(name));
                    break;

                case ILocalSymbol localSymbol:
                    {
                        var localType = localSymbol.Type;
                        resolvedSymbols.AddRange(localType.GetMembers(name));
                        break;
                    }

                case IPropertySymbol propertySymbol:
                    {
                        var propertyType = propertySymbol.Type;
                        resolvedSymbols.AddRange(propertyType.GetMembers(name));
                        break;
                    }
            }
        }

        if (!resolvedSymbols.Any())
        {
            Bind(expression, CandidateReason.NotATypeOrNamespace, []);

            var baseSymbol = baseSymbols.FirstOrDefault();

            if (baseSymbol is INamespaceSymbol namespaceSymbol)
            {
                // TODO: Centralize
                Diagnostics.Add(
                    Diagnostic.Create(
                        CompilerDiagnostics.TypeOrNamespaceNameDoesNotExistInTheNamespace,
                        memberAccessExpression.Name.Identifier.GetLocation(),
                        [name, baseSymbols.First().ToDisplayString()]
                    ));
            }
            else if (baseSymbol is ITypeSymbol typeSymbol)
            {
                // TODO: Centralize
                Diagnostics.Add(
                    Diagnostic.Create(
                        CompilerDiagnostics.MemberDoesNotContainDefinition,
                        memberAccessExpression.Name.Identifier.GetLocation(),
                        [baseSymbols.First().ToDisplayString(), name]
                    ));
            }

            return resolvedSymbols.ToImmutable();
        }

        Bind(memberAccessExpression.Name, resolvedSymbols.First());
        Bind(expression, resolvedSymbols.First());
        return resolvedSymbols.ToImmutable();
    }

    private ImmutableArray<ISymbol> AnalyzeInvocationExpression(ISymbol declaringSymbol, ISymbol containingSymbol, ExpressionSyntax expression, InvocationExpressionSyntax invocationExpression)
    {
        // Analyze the base expression to get potential methods or delegates
        AnalyzeExpression(declaringSymbol, containingSymbol, invocationExpression.Expression, out var baseSymbols);

        if (!baseSymbols.Any())
        {
            return [];
        }

        // Ensure we are invoking a method or delegate
        var candidateMethods = baseSymbols.OfType<IMethodSymbol>().ToList();
        if (!candidateMethods.Any())
        {
            // TODO: Centralize
            Diagnostics.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.MethodNameExpected,
                    invocationExpression.Expression.GetLocation()
                ));

            return [];
        }

        var count = invocationExpression.ArgumentList.Arguments.Count;

        var method = candidateMethods.First(x => x.Parameters.Length == count) as IMethodSymbol;

        // Collect argument types
        var argumentTypes = new List<ITypeSymbol>();
        int i = 0;
        foreach (var argument in invocationExpression.ArgumentList.Arguments)
        {
            AnalyzeExpression(declaringSymbol, declaringSymbol, argument.Expression, out var argSymbols);

            var typeSymbol = argSymbols.First().UnwrapType();

            if (typeSymbol is IErrorTypeSymbol)
            {
                return [];
            }

            if (typeSymbol.SpecialType == SpecialType.System_Void)
            {
                var param = method.Parameters.ElementAt(i);

                // TODO: Centralize
                Diagnostics.Add(
                    Diagnostic.Create(
                        CompilerDiagnostics.CannotConvertFromTypeToType,
                        argument.Expression.GetLocation(),
                        [typeSymbol.Name, param.Type.Name]
                    ));
            }

            argumentTypes.Add(typeSymbol!); // Handle null appropriately in production

            i++;
        }

        // Perform overload resolution
        var bestMethod = ResolveMethodOverload(candidateMethods, argumentTypes);

        if (bestMethod is null)
        {
            // TODO: Centralize
            Diagnostics.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.NoOverloadForMethod,
                    invocationExpression.Expression.GetLocation(),
                    [method.Name, count]
                ));

            Bind(expression, CandidateReason.OverloadResolutionFailure, []);
            return [];
        }
        else
        {
            Bind(expression, bestMethod);
            return [bestMethod.ReturnType];
        }
    }

    private ImmutableArray<ISymbol> AnalyzePredefinedType(PredefinedTypeSyntax predefinedTypeSyntax)
    {
        ImmutableArray<ISymbol> symbols;
        var typeSymbol = _keywordTypeSymbols[predefinedTypeSyntax.Keyword.ValueText];
        symbols = [typeSymbol];
        return symbols;
    }

    private ImmutableArray<ISymbol> AnalyzeIdentifierName(ISymbol declaringSymbol, ISymbol containingSymbol, ExpressionSyntax expression, IdentifierNameSyntax name)
    {
        ImmutableArray<ISymbol> symbols;
        var identifier = name.Identifier.ValueText;

        symbols =
        [
            .. _localSymbols.Where(x => x.Name == identifier),
                .. _imports.Select(x => (x.Key.ToString(), x.Value)).SelectMany(x => x.Value.GetMembers(identifier)),
                .. _symbols.Where(x => x.Name == identifier && (x.ContainingSymbol == containingSymbol || x.ContainingSymbol == declaringSymbol || x.ContainingSymbol == Compilation.GlobalNamespace)),
            ];

        // Fix
        symbols = symbols.DistinctBy(x => x.Name).ToImmutableArray();

        if (symbols.Count() == 1)
        {
            Bind(name, symbols.First());
        }
        else if (symbols.Count() == 0)
        {
            // TODO: Centralize
            Diagnostics.Add(
                Diagnostic.Create(
                    CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext,
                    expression.GetLocation(),
                    [expression.ToString()]
                ));
        }
        else
        {
            Bind(name, CandidateReason.Ambiguous, symbols);
        }

        return symbols;
    }

    private ImmutableArray<ISymbol> AnalyzeLiteralExpression(LiteralExpressionSyntax literalExpression)
    {
        ImmutableArray<ISymbol> symbols;
        switch (literalExpression.Kind)
        {
            case SyntaxKind.NumericLiteralExpression:
                {
                    var symbol = Compilation.GetTypeByMetadataName("System.Int32")!;
                    symbols = [symbol];
                    Bind(literalExpression, symbol);
                    break;
                }

            case SyntaxKind.StringLiteralExpression:
                {
                    var symbol = Compilation.GetTypeByMetadataName("System.String")!;
                    symbols = [symbol];
                    Bind(literalExpression, symbol);
                    break;
                }

            case SyntaxKind.TrueLiteralExpression:
                {
                    var symbol = Compilation.GetTypeByMetadataName("System.Boolean")!;
                    symbols = [symbol];
                    Bind(literalExpression, symbol);
                    break;
                }

            case SyntaxKind.FalseLiteralExpression:
                {
                    var symbol = Compilation.GetTypeByMetadataName("System.Boolean")!;
                    symbols = [symbol];
                    Bind(literalExpression, symbol);
                    break;
                }

            default:
                symbols = [];
                break;
        }

        return symbols;
    }

    private ImmutableArray<ISymbol> AnalyzeBinaryExpression(ISymbol declaringSymbol, ISymbol containingSymbol, BinaryExpressionSyntax binaryExpression)
    {
        ImmutableArray<ISymbol> symbols;
        AnalyzeExpression(declaringSymbol, containingSymbol, binaryExpression.LeftHandSide, out var lhsSymbols);
        AnalyzeExpression(declaringSymbol, containingSymbol, binaryExpression.RightHandSide, out var rhsSymbols);

        if (lhsSymbols.Count() == 1 && rhsSymbols.Count() == 1)
        {
            var lhsSymbol = lhsSymbols.First().UnwrapType();
            var rhsSymbol = rhsSymbols.First().UnwrapType();

            if (lhsSymbol.SpecialType == SpecialType.System_String && rhsSymbol.SpecialType == SpecialType.System_String)
            {
                var symbol = Compilation
                    .GetTypeByMetadataName("System.String")
                    .GetMembers()
                    .OfType<IMethodSymbol>()
                    .Where(x => x.Name == "Concat"
                        && x.Parameters.Count() == 2
                        && x.Parameters[0].Type.SpecialType == SpecialType.System_String
                        && x.Parameters[1].Type.SpecialType == SpecialType.System_String)
                        .First();

                symbols = [symbol.ReturnType];
                Bind(binaryExpression, symbol);
            }
            else if (lhsSymbol == rhsSymbol)
            {
                symbols = [lhsSymbol];
                Bind(binaryExpression, lhsSymbol);
            }
            else
            {
                symbols = [];
                Bind(binaryExpression, CandidateReason.None, []);
            }
        }
        else
        {
            symbols = [];
            Bind(binaryExpression, CandidateReason.None, []);
        }

        return symbols;
    }

    private IMethodSymbol? ResolveMethodOverload(List<IMethodSymbol> candidateMethods, List<ITypeSymbol> argumentTypes)
    {
        IMethodSymbol? bestMatch = null;
        int bestScore = int.MinValue;

        foreach (var method in candidateMethods)
        {
            var parameters = method.Parameters;
            int score = 0;

            if (parameters.Length < argumentTypes.Count)
                continue; // Not enough parameters to match

            for (int i = 0; i < argumentTypes.Count; i++)
            {
                var paramType = parameters[i].Type;

                var argType = argumentTypes[i];

                // Check for exact match or assignable types
                if (argType.Equals(paramType, SymbolEqualityComparer.Default))
                {
                    score += 2; // Exact match
                }
                else if (argType != null && Compilation.ClassifyConversion(argType, paramType).IsImplicit)
                {
                    score += 1; // Implicit conversion
                }
                else
                {
                    score = int.MinValue; // No match
                    break;
                }
            }

            if (score > bestScore)
            {
                bestMatch = method;
                bestScore = score;
            }
        }

        return bestMatch;
    }
}
