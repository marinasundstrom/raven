using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxNodeOrToken
{
    internal readonly GreenNode Green;
    private readonly SyntaxNode _parent;
    private readonly int _index;
    private readonly int _position;

    public SyntaxNodeOrToken(GreenNode node, SyntaxNode parent, int index, int position)
    {
        Green = node ?? throw new ArgumentNullException(nameof(node));
        _parent = parent;
        _index = index;
        _position = position;
    }

    public SyntaxNodeOrToken(SyntaxNode node)
    {
        Green = node.Green;
    }

    public SyntaxNodeOrToken(SyntaxToken token)
    {
        Green = token.Green;
    }

    public bool IsToken => Green is InternalSyntax.SyntaxToken;
    public bool IsNode => Green is InternalSyntax.SyntaxNode;
    public SyntaxToken Token => IsToken ? new SyntaxToken(Green as InternalSyntax.SyntaxToken, _parent, _position) : default;
    public SyntaxNode? Node => IsNode ? Green.CreateRed(_parent, _position) : default;

    public bool AsToken(out SyntaxToken token)
    {
        if (IsToken)
        {
            token = Token;

            return true;
        }

        token = default;
        return false;
    }

    public bool AsNode([NotNullWhen(true)] out SyntaxNode? node)
    {
        if (IsNode)
        {
            node = Node!;

            return true;
        }

        node = null;
        return false;
    }

    public static implicit operator SyntaxNodeOrToken(SyntaxToken token) => new SyntaxNodeOrToken(token);

    public static implicit operator SyntaxNodeOrToken(SyntaxNode node) => new SyntaxNodeOrToken(node);

}