using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides mapping from syntax nodes to declaration keys.
/// </summary>
internal sealed class DeclarationTable
{
    private readonly ConcurrentDictionary<SyntaxNode, DeclKey> _map = new();

    public DeclarationTable(IEnumerable<SyntaxTree> syntaxTrees)
    {
        foreach (var tree in syntaxTrees)
            Build(tree.GetRoot());
    }

    private void Build(SyntaxNode node)
    {
        if (TryCreateDeclKey(node, out var key))
            _map[node] = key;

        foreach (var child in node.ChildNodes())
            Build(child);
    }

    public bool TryGetDeclKey(SyntaxNode node, out DeclKey key)
        => _map.TryGetValue(node, out key);

    private bool TryCreateDeclKey(SyntaxNode node, out DeclKey key)
    {
        switch (node)
        {
            case ClassDeclarationSyntax cls:
                key = CreateKey(SymbolKind.Type, cls.Identifier.ValueText, 0, cls);
                return true;
            case ExtensionDeclarationSyntax extension:
                key = CreateKey(SymbolKind.Type, extension.Identifier.ValueText, 0, extension);
                return true;
            case MethodDeclarationSyntax method:
                key = CreateKey(SymbolKind.Method, method.Identifier.ValueText, 0, method);
                return true;
            case VariableDeclaratorSyntax { Parent.Parent: LocalDeclarationStatementSyntax } variable:
                key = CreateKey(SymbolKind.Local, variable.Identifier.ValueText, 0, variable);
                return true;
            default:
                key = default;
                return false;
        }
    }

    private DeclKey CreateKey(SymbolKind kind, string name, int arity, SyntaxNode node)
    {
        var containing = GetContainingDeclKey(node.Parent);
        return new DeclKey(kind, name, arity, containing, ImmutableArray.Create(node.GetLocation()));
    }

    private DeclKey? GetContainingDeclKey(SyntaxNode? node)
    {
        while (node is not null)
        {
            if (_map.TryGetValue(node, out var key))
                return key;
            if (TryCreateDeclKey(node, out var created))
            {
                _map[node] = created;
                return created;
            }
            node = node.Parent;
        }
        return null;
    }
}
