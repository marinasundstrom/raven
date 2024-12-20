using System.Collections.Immutable;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class SemanticModel
{
    private readonly DiagnosticBag Diagnostics;
    private readonly List<ISymbol> _symbols = new List<ISymbol>();
    private readonly List<ISymbol> _localSymbols = new List<ISymbol>();

    public SemanticModel(Compilation compilation, List<ISymbol> symbols, SyntaxTree syntaxTree, DiagnosticBag diagnostics)
    {
        _symbols = symbols;
        Diagnostics = diagnostics;
        Compilation = compilation;
        SyntaxTree = syntaxTree;

        CreateModel();
    }

    private void CreateModel()
    {
        foreach (var symbol in _symbols.OfType<SourceMethodSymbol>())
        {
            var syntaxRef  = symbol.DeclaringSyntaxReferences.First();
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
                    declarator.Name.ToString(), returnType, declaringSymbol!, null, null,
                    locations, references);

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
            AnalyzeExpression(declaringSymbol, ifStatement.Condition);

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
                AnalyzeExpression(declaringSymbol, returnStatement.Expression);
            }
        }
        else if (statement is ExpressionStatementSyntax expressionStatement)
        {
            AnalyzeExpression(declaringSymbol, expressionStatement.Expression);
        }
    }

    private void AnalyzeExpression(ISymbol declaringSymbol, ExpressionSyntax expression)
    {
        //Console.WriteLine(expression.SyntaxTree);
    }

    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        var symbols = _symbols.Concat(_localSymbols).Where(x => x.DeclaringSyntaxReferences.Any(x2 => x2.GetSyntax() == node));
        if (symbols.Count() == 1)
        {
            return new SymbolInfo(symbols.First());
        }
        return new SymbolInfo(CandidateReason.None, [.. _symbols]);
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