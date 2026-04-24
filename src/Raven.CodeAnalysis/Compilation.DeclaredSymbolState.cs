using System.Collections.Concurrent;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, SourceNamedTypeSymbol> _declaredTypeSymbols = new();
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, SourceNamedTypeSymbol> _classSymbols = new();
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, SourceUnionSymbol> _unionSymbols = new();
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, SourceUnionCaseTypeSymbol> _unionCaseSymbols = new();
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, IMethodSymbol> _methodSymbols = new();

    internal void RegisterDeclaredTypeSymbol(SyntaxNode node, SourceNamedTypeSymbol symbol)
        => _declaredTypeSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal bool TryGetDeclaredTypeSymbol(SyntaxNode node, out SourceNamedTypeSymbol symbol)
        => _declaredTypeSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterClassSymbol(TypeDeclarationSyntax node, SourceNamedTypeSymbol symbol)
        => _classSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal SourceNamedTypeSymbol GetClassSymbol(TypeDeclarationSyntax node)
        => _classSymbols[CreateDeclaredSyntaxKey(node)];

    internal bool TryGetClassSymbol(TypeDeclarationSyntax node, out SourceNamedTypeSymbol symbol)
        => _classSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterUnionSymbol(UnionDeclarationSyntax node, SourceUnionSymbol symbol)
        => _unionSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal SourceUnionSymbol GetUnionSymbol(UnionDeclarationSyntax node)
        => _unionSymbols[CreateDeclaredSyntaxKey(node)];

    internal bool TryGetUnionSymbol(UnionDeclarationSyntax node, out SourceUnionSymbol symbol)
        => _unionSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterUnionCaseSymbol(CaseDeclarationSyntax node, SourceUnionCaseTypeSymbol symbol)
        => _unionCaseSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal SourceUnionCaseTypeSymbol GetUnionCaseSymbol(CaseDeclarationSyntax node)
        => _unionCaseSymbols[CreateDeclaredSyntaxKey(node)];

    internal bool TryGetUnionCaseSymbol(CaseDeclarationSyntax node, out SourceUnionCaseTypeSymbol symbol)
        => _unionCaseSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterMethodSymbol(MethodDeclarationSyntax node, IMethodSymbol symbol)
        => _methodSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal bool TryGetMethodSymbol(MethodDeclarationSyntax node, out IMethodSymbol symbol)
        => _methodSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    private static DeclaredSyntaxKey CreateDeclaredSyntaxKey(SyntaxNode node)
        => new(node.SyntaxTree, node.Span, node.Kind);

    private readonly record struct DeclaredSyntaxKey(SyntaxTree SyntaxTree, Text.TextSpan Span, SyntaxKind Kind);
}
