using System.Collections.Immutable;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class SemanticModel
{
    List<ISymbol> _symbols = new List<ISymbol>();

    public SemanticModel(Compilation compilation, SyntaxTree syntaxTree)
    {
        Compilation = compilation;
        SyntaxTree = syntaxTree;

        CreateModel();
    }

    private void CreateModel()
    {
        var root = SyntaxTree.GetRoot();

        Location[] locations = [SyntaxTree.GetLocation(root.Span)];

        SyntaxReference[] references = [new SyntaxReference(SyntaxTree, root.Span)];

        var symbol = new SourceNamespaceSymbol(
            "global", null!, null, null,
            locations, references);

        foreach (var memberDeclaration in root.Members)
        {
            AnalyzeMemberDeclaration(symbol, memberDeclaration);
        }

        _symbols.Add(symbol);
    }

    private void AnalyzeMemberDeclaration(ISymbol declaringSymbol, MemberDeclarationSyntax memberDeclaration)
    {
        if (memberDeclaration is MethodDeclarationSyntax methodDeclaration)
        {
            Location[] locations = [SyntaxTree.GetLocation(methodDeclaration.Span)];

            SyntaxReference[] references = [new SyntaxReference(SyntaxTree, methodDeclaration.Span)];

            ITypeSymbol typeSymbol = null!;

            var symbol = new SourceMethodSymbol(
                methodDeclaration.Name.ToString(), typeSymbol, null!, null, null,
                locations, references);

            _symbols.Add(symbol);
        }
        else if (memberDeclaration is GlobalStatementSyntax globalStatement)
        {
            var statement = globalStatement.Statement;
            AnalyzeStatement(declaringSymbol, statement);
        }
    }

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

                _symbols.Add(symbol);
            }
        }
        else if (statement is BlockSyntax block)
        {
            foreach (var s in block.Statements)
            {
                var x = s == s;

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
        Console.WriteLine(expression.SyntaxTree);
    }

    public Compilation Compilation { get; }

    public SyntaxTree SyntaxTree { get; }

    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        var symbols = _symbols.Where(x => x.DeclaringSyntaxReferences.Any(x2 => x2.GetSyntax() == node));
        if (symbols.Count() == 1)
        {
            return new SymbolInfo(symbols.First());
        }
        return new SymbolInfo(CandidateReason.None, [.. _symbols]);
    }

    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        return _symbols.FirstOrDefault(x => x.DeclaringSyntaxReferences.Any(x2 => x2.GetSyntax() == node));
    }

    public ImmutableArray<ISymbol> LookupSymbols(int position,
        INamespaceOrTypeSymbol container, string name, bool includeReducedExtensionMethods)
    {
        return default!;
    }
}