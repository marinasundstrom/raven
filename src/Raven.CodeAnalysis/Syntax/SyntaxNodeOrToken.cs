using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxNodeOrToken
{
    internal readonly GreenNode Green;
    private readonly SyntaxNode _parent;
    private readonly int _position;
    private SyntaxToken? _token;
    private SyntaxNode _node;

    internal SyntaxNodeOrToken(GreenNode node, SyntaxNode parent, int index, int position)
    {
        Green = node ?? throw new ArgumentNullException(nameof(node));
        _parent = parent;
        _position = position;
    }

    public SyntaxNodeOrToken(SyntaxNode node)
    {
        Green = node.Green;
        _node = node;
    }

    public SyntaxNodeOrToken(SyntaxToken token)
    {
        Green = token.Green;
        _token = token;
    }

    public SyntaxNode Parent => _parent ?? _token?.Parent ?? _node.Parent;

    public bool IsToken => Green is InternalSyntax.SyntaxToken;
    public bool IsNode => Green is InternalSyntax.SyntaxNode;

    public SyntaxToken AsToken() => IsToken ? _token ??= new SyntaxToken(Green as InternalSyntax.SyntaxToken, _parent, _position) : default;

    public SyntaxNode? AsNode()
    {
        if (_node is not null)
        {
            return _node;
        }

        int position = _position;
        SyntaxNode? parent = _parent;
        GreenNode? green = Green;

        return IsNode ? _node = SyntaxNodeCache.GetValue(Green, (s) => s.CreateRed(parent, position)) : default;
    }


    public bool TryGetToken(out SyntaxToken token)
    {
        if (IsToken)
        {
            token = AsToken();

            return true;
        }

        token = default;
        return false;
    }

    public bool TryGetNode([NotNullWhen(true)] out SyntaxNode? node)
    {
        if (IsNode)
        {
            node = AsNode()!;

            return true;
        }

        node = null;
        return false;
    }

    public static implicit operator SyntaxNodeOrToken(SyntaxToken token) => new SyntaxNodeOrToken(token);

    public static implicit operator SyntaxNodeOrToken(SyntaxNode node) => new SyntaxNodeOrToken(node);

    public override string ToString()
    {
        return $"{(AsNode()?.ToFullString() ?? AsToken().Text)}";
    }

}