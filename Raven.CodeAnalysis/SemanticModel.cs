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
    private readonly Dictionary<SyntaxNode, ImmutableArray<ISymbol>> _bindings = new();

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

    public IImmutableList<Diagnostic> GetDiagnostics() => _diagnostics.ToImmutableArray();

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
                    AnalyzeExpression(declaringSymbol, declarator.Initializer.Value, out var x);
                }

                _bindings[declarator] = [symbol];
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
            AnalyzeExpression(declaringSymbol, ifStatement.Condition, out var s);

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
                AnalyzeExpression(declaringSymbol, returnStatement.Expression, out var s);
            }
        }
        else if (statement is ExpressionStatementSyntax expressionStatement)
        {
            AnalyzeExpression(declaringSymbol, expressionStatement.Expression, out var s);
        }
    }

    private void AnalyzeExpression(ISymbol declaringSymbol, ExpressionSyntax expression, out ImmutableArray<ISymbol> symbols)
    {
        if (expression is MemberAccessExpressionSyntax memberAccessExpression)
        {
            AnalyzeExpression(declaringSymbol, memberAccessExpression.Expression, out var baseSymbols);

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
                Diagnostics.Add(
                    Diagnostic.Create(
                        CompilerDiagnostics.TypeNameDoesNotExistInType,
                        memberAccessExpression.Name.Identifier.GetLocation(),
                        [name, baseSymbols.First().ToDisplayString()]
                    ));
            }

            symbols = resolvedSymbols.ToImmutable();
            _bindings[expression] = symbols;
        }
        else if (expression is InvocationExpressionSyntax invocationExpression)
        {
            AnalyzeExpression(declaringSymbol, invocationExpression.Expression, out var baseSymbols);

            if (!baseSymbols.Any())
            {
                symbols = [];
                return;
            }

            if (!baseSymbols.Any(x => x.Kind == SymbolKind.Method || x.Kind == SymbolKind.Field))
            {
                var expr = invocationExpression.Expression;

                if (expr is MemberAccessExpressionSyntax)
                {
                    expr = ((MemberAccessExpressionSyntax)expr).Name;
                }

                Diagnostics.Add(
                    Diagnostic.Create(
                        CompilerDiagnostics.MethodNameExpected,
                        expr.GetLocation()
                    ));

                symbols = [];
                return;
            }

            // Is invocable: Method or Delegate
            // Is overloaded?

            symbols = [.. baseSymbols.OfType<IMethodSymbol>()];
            _bindings[expression] = symbols;
        }
        else if (expression is IdentifierNameSyntax name)
        {
            var identifier = name.Identifier.ValueText;
            symbols =
            [
                .. _localSymbols.Where(x => x.Name == identifier),
                .. _symbols.Where(x => x.Name == identifier),
            ];
            _bindings[name] = symbols;
        }
        else
        {
            symbols = ImmutableArray<ISymbol>.Empty;
        }
    }

    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        if (_bindings.TryGetValue(node, out var symbols))
        {
            if (symbols.Length == 1)
            {
                return new SymbolInfo(symbols[0]);
            }
            return new SymbolInfo(CandidateReason.OverloadResolutionFailure, symbols);
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