using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Metadata;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class SemanticModel
{
    private readonly DiagnosticBag _diagnostics;
    private readonly List<ISymbol> _symbols = new List<ISymbol>();
    private readonly List<ISymbol> _localSymbols = new List<ISymbol>();
    private readonly Dictionary<SyntaxNode, SymbolInfo> _bindings = new();
    private readonly Dictionary<TypeSyntax, INamespaceSymbol> _imports = new();
    private readonly Dictionary<string, ITypeSymbol> _keywordTypeSymbols = new();

    public SemanticModel(Compilation compilation, List<ISymbol> symbols, SyntaxTree syntaxTree)
    {
        _symbols = symbols;
        _diagnostics = new DiagnosticBag();
        Compilation = compilation;
        SyntaxTree = syntaxTree;

        _keywordTypeSymbols = new Dictionary<string, ITypeSymbol>
        {
            { "int", Compilation.GetSpecialType(SpecialType.System_Int32) },
            { "bool", Compilation.GetSpecialType(SpecialType.System_Boolean) },
            { "char", Compilation.GetSpecialType(SpecialType.System_Char) },
            { "string", Compilation.GetSpecialType(SpecialType.System_String) }
        };

        CreateModel();
    }

    private void CreateModel()
    {
        foreach (var symbol in _symbols.OfType<SourceMethodSymbol>())
        {
            var syntaxRef = symbol.DeclaringSyntaxReferences.First();
            var syntax = syntaxRef.GetSyntax();

            if (syntax is MethodDeclarationSyntax methodDeclaration)
            {

            }
            else if (syntax is CompilationUnitSyntax compilationUnit)
            {
                foreach (var import in compilationUnit.Imports)
                {
                    var namespaceSymbol = Compilation.GetNamespaceSymbol(import.Namespace.ToString());
                    _imports.Add(import.Namespace, namespaceSymbol);
                }

                foreach (var member in compilationUnit.Members)
                {
                    if (member is GlobalStatementSyntax globalStatement)
                    {
                        AnalyzeStatement(symbol, globalStatement.Statement);
                    }
                }
            }
        }
    }

    public Compilation Compilation { get; }

    public SyntaxTree SyntaxTree { get; }

    private DiagnosticBag Diagnostics => _diagnostics;

    public IImmutableList<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default) => _diagnostics.ToImmutableArray();

    private void AnalyzeStatement(ISymbol declaringSymbol, StatementSyntax statement)
    {
        if (statement is LocalDeclarationStatementSyntax localDeclarationStatement)
        {
            foreach (var declarator in localDeclarationStatement.Declaration.Declarators)
            {
                Location[] locations = [SyntaxTree.GetLocation(declarator.Span)];

                SyntaxReference[] references = [new SyntaxReference(SyntaxTree, declarator.Span)];

                ITypeSymbol propertyType = null;

                var typeExpr = declarator?.TypeAnnotation?.Type;

                ImmutableArray<ISymbol> expSymbols = [];

                if (declarator?.Initializer?.Value is not null)
                {
                    AnalyzeExpression(declaringSymbol, declaringSymbol, declarator.Initializer.Value, out expSymbols);

                    if (!expSymbols.Any())
                        return;

                }

                if (typeExpr is not null)
                {
                    propertyType = ResolveType(typeExpr);
                    Bind(typeExpr!, propertyType);
                }
                else
                {
                    propertyType = expSymbols.FirstOrDefault() as ITypeSymbol;
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
                            var z = Compilation.ClassifyConversion(typeSymbol, propertyType);
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
        else if (statement is BlockSyntax block)
        {
            foreach (var s in block.Statements)
            {
                AnalyzeStatement(declaringSymbol, s);
            }
        }
        else if (statement is IfStatementSyntax ifStatement)
        {
            AnalyzeExpression(declaringSymbol, declaringSymbol, ifStatement.Condition, out var s);

            AnalyzeStatement(declaringSymbol, ifStatement.Statement);

            if (ifStatement.ElseClause is not null)
            {
                AnalyzeStatement(declaringSymbol, ifStatement.ElseClause.Statement);
            }
        }
        else if (statement is ReturnStatementSyntax returnStatement)
        {
            if (returnStatement.Expression is not null)
            {
                AnalyzeExpression(declaringSymbol, declaringSymbol, returnStatement.Expression, out var s);
            }
        }
        else if (statement is ExpressionStatementSyntax expressionStatement)
        {
            AnalyzeExpression(declaringSymbol, declaringSymbol, expressionStatement.Expression, out var s);
        }
    }

    private ITypeSymbol? ResolveType(TypeSyntax? typeExpr)
    {
        var typeName = typeExpr.ToString();
        return Compilation.GetTypeByMetadataName(typeName);
    }

    private void AnalyzeExpression(ISymbol declaringSymbol, ISymbol containingSymbol, ExpressionSyntax expression, out ImmutableArray<ISymbol> symbols)
    {
        if (expression is MemberAccessExpressionSyntax memberAccessExpression)
        {
            AnalyzeExpression(declaringSymbol, containingSymbol, memberAccessExpression.Expression, out var baseSymbols);

            var name = memberAccessExpression.Name.Identifier.Text;

            var resolvedSymbols = ImmutableArray.CreateBuilder<ISymbol>();

            foreach (var baseSymbol in baseSymbols)
            {
                if (baseSymbol is INamespaceSymbol namespaceSymbol)
                {
                    resolvedSymbols.AddRange(namespaceSymbol.GetMembers(name));
                }
                else if (baseSymbol is ITypeSymbol typeSymbol)
                {
                    resolvedSymbols.AddRange(typeSymbol.GetMembers(name));
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
                            [name, baseSymbols.First().ToDisplayString()]
                        ));
                }

                symbols = resolvedSymbols.ToImmutable();
                return;
            }

            symbols = resolvedSymbols.ToImmutable();
            Bind(expression, symbols.First());
        }
        else if (expression is InvocationExpressionSyntax invocationExpression)
        {
            // Analyze the base expression to get potential methods or delegates
            AnalyzeExpression(declaringSymbol, containingSymbol, invocationExpression.Expression, out var baseSymbols);

            if (!baseSymbols.Any())
            {
                symbols = ImmutableArray<ISymbol>.Empty;
                return;
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

                symbols = ImmutableArray<ISymbol>.Empty;
                return;
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

                symbols = ImmutableArray<ISymbol>.Empty;
                Bind(expression, CandidateReason.OverloadResolutionFailure, symbols);
            }
            else
            {
                symbols = [bestMethod.ReturnType];
                Bind(expression, bestMethod);
            }
        }
        else if (expression is PredefinedTypeSyntax predefinedTypeSyntax)
        {
            var typeSymbol = _keywordTypeSymbols[predefinedTypeSyntax.Keyword.ValueText];
            symbols = [typeSymbol];
        }
        else if (expression is IdentifierNameSyntax name)
        {
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
        }
        else if (expression is LiteralExpressionSyntax literalExpression)
        {
            if (literalExpression.Kind == SyntaxKind.StringLiteralExpression)
            {
                var symbol = Compilation.GetTypeByMetadataName("System.String")!;
                symbols = [symbol];
                Bind(literalExpression, symbol);
            }
            else if (literalExpression.Kind == SyntaxKind.TrueLiteralExpression)
            {
                var symbol = Compilation.GetTypeByMetadataName("System.Boolean")!;
                symbols = [symbol];
                Bind(literalExpression, symbol);
            }
            else if (literalExpression.Kind == SyntaxKind.FalseLiteralExpression)
            {
                var symbol = Compilation.GetTypeByMetadataName("System.Boolean")!;
                symbols = [symbol];
                Bind(literalExpression, symbol);
            }
            else
            {
                symbols = [];
            }
        }
        else if (expression is BinaryExpressionSyntax binaryExpression)
        {
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
        }
        else
        {
            symbols = ImmutableArray<ISymbol>.Empty;
        }
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

    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        if (_bindings.TryGetValue(node, out var symbolInfo))
        {
            return symbolInfo;
        }

        return new SymbolInfo(CandidateReason.None, ImmutableArray<ISymbol>.Empty);
    }

    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        return _symbols.Concat(_localSymbols).FirstOrDefault(x => x.DeclaringSyntaxReferences.Any(x2 => x2.GetSyntax() == node));
    }

    public ImmutableArray<ISymbol> LookupSymbols(int position,
        INamespaceOrTypeSymbol container, string name, bool includeReducedExtensionMethods)
    {
        throw new NotImplementedException();
    }

    private void Bind(SyntaxNode node, ISymbol symbol)
    {
        _bindings[node] = new SymbolInfo(symbol);
    }

    private void Bind(SyntaxNode node, CandidateReason reason, params IEnumerable<ISymbol> symbols)
    {
        _bindings[node] = new SymbolInfo(reason, symbols.ToImmutableArray());
    }

    public TypeInfo GetTypeInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        // Get the symbol associated with the syntax node
        var symbolInfo = GetSymbolInfo(node).Symbol;

        // If a symbol is found, attempt to determine its type
        if (symbolInfo is not null)
        {
            // Handle specific types of symbols
            if (symbolInfo is ITypeSymbol typeSymbol)
            {
                // Directly return the TypeInfo for type symbols
                return new TypeInfo(typeSymbol, null);
            }
            else if (symbolInfo is IMethodSymbol methodSymbol)
            {
                // Return the return type of the method
                return new TypeInfo(methodSymbol.ReturnType, null);
            }
            else if (symbolInfo is IPropertySymbol propertySymbol)
            {
                // Return the type of the property
                return new TypeInfo(propertySymbol.Type, null);
            }
            else if (symbolInfo is IFieldSymbol fieldSymbol)
            {
                // Return the type of the field
                return new TypeInfo(fieldSymbol.Type, null);
            }
        }

        // Handle cases where no symbol is found
        if (node is LiteralExpressionSyntax literalExpression)
        {
            // Infer the type of the literal
            var literalType = InferLiteralType(literalExpression);
            return new TypeInfo(literalType, null);
        }

        // If no type could be determined, return null TypeInfo
        return new TypeInfo(null, null);
    }

    private ITypeSymbol? InferLiteralType(LiteralExpressionSyntax literal)
    {
        // Example inference logic for literals
        return literal.Token.Value switch
        {
            int => GetTypeSymbol("System.Int32"),
            string => GetTypeSymbol("System.String"),
            double => GetTypeSymbol("System.Double"),
            bool => GetTypeSymbol("System.Boolean"),
            _ => null
        };
    }

    private ITypeSymbol? GetTypeSymbol(string typeName)
    {
        return _symbols
            .OfType<ITypeSymbol>()
            .FirstOrDefault(x => x.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat) == typeName);
    }
}

public class TypeInfo
{
    internal TypeInfo(ITypeSymbol type, ITypeSymbol? convertedType)
    {
        Type = type;
        ConvertedType = convertedType;
    }

    public NullabilityInfo ConvertedNullability { get; }

    public ITypeSymbol? ConvertedType { get; }

    public NullabilityInfo Nullability { get; }

    public ITypeSymbol? Type { get; }
}

[System.Diagnostics.DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public readonly struct NullabilityInfo : IEquatable<NullabilityInfo>
{
    public NullableAnnotation Annotation { get; }

    public NullableFlowState FlowState { get; }

    public bool Equals(NullabilityInfo other)
    {
        throw new NotImplementedException();
    }
}

public enum NullableAnnotation
{
    None,
    NotAnnotated,
    Annotated
}

public enum NullableFlowState
{
    None,
    NotNull,
    MaybeNull
}

public static class SymbolExtensions
{
    public static ITypeSymbol? UnwrapType(this ISymbol symbol)
    {
        if (symbol is IPropertySymbol propertySymbol) return propertySymbol.Type;

        if (symbol is ILocalSymbol localSymbol) return localSymbol.Type;

        return symbol as ITypeSymbol;
    }
}