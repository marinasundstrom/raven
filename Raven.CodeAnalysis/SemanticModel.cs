using System.Collections.Immutable;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class SemanticModel
{
    //Dictionary<SyntaxNode, List<ISymbol>> syntaxNodeSymbolMappings = new Dictionary();

    public SemanticModel(Compilation compilation, SyntaxTree syntaxTree)
    {
        Compilation = compilation;
        SyntaxTree = syntaxTree;
    }

    public void CreateModel()
    {
        var root = SyntaxTree.GetSyntaxRoot();

        foreach (var memberDeclaration in root.Members)
        {
            AnalyzeMemberDeclaration(memberDeclaration);
        }
    }

    private void AnalyzeMemberDeclaration(MemberDeclarationSyntax memberDeclaration)
    {

    }

    public Compilation Compilation { get; }

    public SyntaxTree SyntaxTree { get; }

    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        return new SymbolInfo((ISymbol)null);
    }

    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        return null;
    }

    public ImmutableArray<ISymbol> LookupSymbols(TextSpan span)
    {
        return default!;
    }
}