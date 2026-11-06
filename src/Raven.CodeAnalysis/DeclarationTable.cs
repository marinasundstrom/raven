using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides mapping from syntax nodes to declaration keys.
/// </summary>
internal sealed class DeclarationTable
{
    private readonly Dictionary<SyntaxNodeMapKey, DeclKey> _map = new();

    public DeclarationTable(IEnumerable<SyntaxTree> syntaxTrees, DeclarationTable? previous = null)
    {
        var treesToBuild = new HashSet<SyntaxTree>(syntaxTrees);

        if (previous is not null)
            previous.CopyEntriesForTrees(treesToBuild, _map);

        foreach (var tree in treesToBuild)
            Build(tree.GetRoot(), containing: null);
    }

    private void Build(SyntaxNode node, DeclKey? containing)
    {
        var key = CreateDeclarationKey(node, containing);
        if (key is not null)
            _map[GetKey(node)] = key;

        foreach (var child in node.ChildNodes())
            Build(child, key ?? containing);
    }

    public bool TryGetDeclKey(SyntaxNode node, [NotNullWhen(true)] out DeclKey? key)
    {
        if (node.SyntaxTree is null)
        {
            key = null;
            return false;
        }

        return _map.TryGetValue(GetKey(node), out key);
    }

    private static DeclKey? CreateDeclarationKey(SyntaxNode node, DeclKey? containing)
    {
        return node switch
        {
            ClassDeclarationSyntax cls => CreateType(cls.Identifier, cls.TypeParameterList),
            RecordDeclarationSyntax record => CreateType(record.Identifier, record.TypeParameterList),
            StructDeclarationSyntax st => CreateType(st.Identifier, st.TypeParameterList),
            UnionDeclarationSyntax union => CreateType(union.Identifier, union.TypeParameterList),
            ExtensionDeclarationSyntax extension => CreateType(extension.Identifier, extension.TypeParameterList),
            MethodDeclarationSyntax method => Create(SymbolKind.Method, method.Identifier, method.TypeParameterList?.Parameters.Count ?? 0),
            VariableDeclaratorSyntax
            {
                Identifier.Text: "_",
                Parent: VariableDeclarationSyntax { BindingKeyword.Kind: SyntaxKind.LetKeyword or SyntaxKind.ValKeyword }
            } => null,
            VariableDeclaratorSyntax { Parent.Parent: LocalDeclarationStatementSyntax } local => Create(SymbolKind.Local, local.Identifier, 0),
            _ => null
        };

        DeclKey CreateType(SyntaxToken name, TypeParameterListSyntax? parameters) =>
            Create(SymbolKind.Type, name, parameters?.Parameters.Count ?? 0);

        DeclKey Create(SymbolKind kind, SyntaxToken name, int arity) =>
            new(kind, name.ValueText, arity, containing, ImmutableArray.Create(node.GetLocation()));
    }

    private static SyntaxNodeMapKey GetKey(SyntaxNode node)
        => new(
            node.SyntaxTree ?? throw new InvalidOperationException("Detached syntax nodes cannot be declaration keys."),
            node.Span,
            node.Kind);

    private void CopyEntriesForTrees(
        HashSet<SyntaxTree> treesToReuse,
        Dictionary<SyntaxNodeMapKey, DeclKey> destination)
    {
        if (treesToReuse.Count == 0)
            return;

        var reusedTrees = new HashSet<SyntaxTree>();

        foreach (var (key, value) in _map)
        {
            if (!treesToReuse.Contains(key.SyntaxTree))
                continue;

            destination[key] = value;
            reusedTrees.Add(key.SyntaxTree);
        }

        treesToReuse.ExceptWith(reusedTrees);
    }

    private readonly record struct SyntaxNodeMapKey(SyntaxTree SyntaxTree, TextSpan Span, SyntaxKind Kind);
}
