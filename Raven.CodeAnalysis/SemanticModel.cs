using System.Collections.Immutable;
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

    public SemanticModel(Compilation compilation, List<ISymbol> symbols, SyntaxTree syntaxTree)
    {
        _symbols = symbols;
        _diagnostics = new DiagnosticBag();
        Compilation = compilation;
        SyntaxTree = syntaxTree;

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

                ITypeSymbol returnType = null!;

                var symbol = new SourceLocalSymbol(
                    declarator.Name.Identifier.Text.ToString(), returnType, declaringSymbol!, declaringSymbol.ContainingType, declaringSymbol.ContainingNamespace,
                    locations, references);

                if (declarator?.Initializer?.Value is not null)
                {
                    AnalyzeExpression(declaringSymbol, declaringSymbol, declarator.Initializer.Value, out var x);
                }

                _bindings[declarator] = new SymbolInfo(symbol);
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
                _bindings[expression] = new SymbolInfo(CandidateReason.NotATypeOrNamespace, []);

                // TODO: Centralize
                Diagnostics.Add(
                    Diagnostic.Create(
                        CompilerDiagnostics.TypeNameDoesNotExistInType,
                        memberAccessExpression.Name.Identifier.GetLocation(),
                        [name, baseSymbols.First().ToDisplayString()]
                    ));

                symbols = resolvedSymbols.ToImmutable();
                return;
            }

            symbols = resolvedSymbols.ToImmutable();
            _bindings[expression] = new SymbolInfo(symbols.First());
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

            // Collect argument types
            var argumentTypes = new List<ITypeSymbol>();
            foreach (var argument in invocationExpression.ArgumentList.Arguments)
            {
                AnalyzeExpression(declaringSymbol, declaringSymbol, argument.Expression, out var argSymbols);
                var typeSymbol = argSymbols.OfType<ITypeSymbol>().FirstOrDefault();
                if (typeSymbol is null)
                {
                    typeSymbol = argSymbols.OfType<IMethodSymbol>().FirstOrDefault()?.ReturnType;
                }
                argumentTypes.Add(typeSymbol!); // Handle null appropriately in production
            }

            // Perform overload resolution
            var bestMethod = ResolveMethodOverload(candidateMethods, argumentTypes);

            if (bestMethod is null)
            {
                // TODO: Centralize
                Diagnostics.Add(
                    Diagnostic.Create(
                        CompilerDiagnostics.NoOverloadForMethod,
                        invocationExpression.Expression.GetLocation()
                    ));

                symbols = ImmutableArray<ISymbol>.Empty;

                _bindings[expression] = new SymbolInfo(CandidateReason.OverloadResolutionFailure, symbols);
            }
            else
            {
                symbols = [bestMethod.ReturnType];
                _bindings[expression] = new SymbolInfo(bestMethod);
            }
        }
        else if (expression is IdentifierNameSyntax name)
        {
            var identifier = name.Identifier.ValueText;
            symbols =
            [
                .. _localSymbols.Where(x => x.Name == identifier),
                .. _symbols.Where(x => x.Name == identifier && (x.ContainingSymbol == containingSymbol || x.ContainingSymbol == declaringSymbol || x.ContainingSymbol == Compilation.GlobalNamespace)),
            ];

            if (symbols.Count() == 1)
            {
                _bindings[name] = new SymbolInfo(symbols.First());
            }
            else
            {
                _bindings[name] = new SymbolInfo(CandidateReason.Ambiguous, symbols);
            }
        }
        else if (expression is LiteralExpressionSyntax literalExpression)
        {
            if (literalExpression.Kind == SyntaxKind.StringLiteralExpression)
            {
                var symbol = Compilation.GetTypeByMetadataName("System.String")!;
                symbols = [symbol];

                _bindings[literalExpression] = new SymbolInfo(symbol);
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
                var lhsSymbol = lhsSymbols.First() as ITypeSymbol;
                var rhsSymbol = rhsSymbols.First() as ITypeSymbol;

                if (lhsSymbol.SpecialType == SpecialType.System_String && rhsSymbol.SpecialType == SpecialType.System_String)
                {
                    var symbol = Compilation
                        .GetTypeByMetadataName("System.String")
                        .GetMembers()
                        .OfType<IMethodSymbol>()
                        .Where(x => x.Name == "Concat"
                            && x.Parameters.Count() == 2
                            && x.Parameters[0].Type.SpecialType == SpecialType.System_String
                            && x.Parameters[1].Type.SpecialType == SpecialType.System_String);

                    symbols = [symbol.First()];

                    _bindings[binaryExpression] = new SymbolInfo(symbol.First());
                }
                else
                {
                    symbols = [];

                    _bindings[binaryExpression] = new SymbolInfo(CandidateReason.None, symbols);
                }
            }
            else
            {
                symbols = [];

                _bindings[binaryExpression] = new SymbolInfo(CandidateReason.None, symbols);
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
        return default!;
    }
}