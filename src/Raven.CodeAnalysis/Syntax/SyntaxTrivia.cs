using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxTrivia
{
    internal readonly InternalSyntax.SyntaxTrivia Green;
    private readonly SyntaxToken? _token;
    private readonly int _position;
    private SyntaxNode? _structure;

    public SyntaxKind Kind => Green.Kind;

    public string Text => Green.Text;

    public SyntaxToken? Token => _token;

    public TextSpan Span
    {
        get
        {
            return new TextSpan(_position, Green.Width);
        }
    }

    public int SpanStart
    {
        get
        {
            return _position;
        }
    }

    public Location GetLocation()
    {
        var syntaxTree = Token?.SyntaxTree;
        if (syntaxTree is null)
        {
            return default(Location)!;
        }
        return syntaxTree.GetLocation(Span);
    }

    internal SyntaxTrivia(InternalSyntax.SyntaxTrivia trivia, SyntaxToken parent, int position = 0)
    {
        Green = trivia ?? throw new ArgumentNullException(nameof(trivia));
        _token = parent;
        _position = position;
    }

    public SyntaxTrivia(SyntaxKind kind)
    {
        Green = new InternalSyntax.SyntaxTrivia(kind, SyntaxFacts.GetSyntaxTokenText(kind)!);
        _token = null!;
    }

    public SyntaxNode? GetStructure()
    {
        var tokenParent = _token?.Parent;
        var _parent = tokenParent?.Parent;
        return _structure ??= Green.GetStructuredTrivia()?.CreateRed(tokenParent, _position);
    }

    public bool HasStructure => Green.HasStructuredTrivia;

    public override string ToString()
    {
        if (HasStructure)
        {
            return GetStructure()!.ToString();
        }
        return Text;
    }
}

public static partial class SyntaxFactory
{
    public static SyntaxTrivia Whitespace(string text) => (SyntaxTrivia)InternalSyntax.SyntaxFactory.Whitespace(text);
    public static readonly SyntaxTrivia LineFeed = (SyntaxTrivia)InternalSyntax.SyntaxFactory.LineFeed;
    public static readonly SyntaxTrivia CarriageReturn = (SyntaxTrivia)InternalSyntax.SyntaxFactory.CarriageReturn;
    public static readonly SyntaxTrivia CarriageReturnLineFeed = (SyntaxTrivia)InternalSyntax.SyntaxFactory.CarriageReturnLineFeed;
    public static readonly SyntaxTrivia Space = (SyntaxTrivia)InternalSyntax.SyntaxFactory.Space;
    public static readonly SyntaxTrivia Tab = (SyntaxTrivia)InternalSyntax.SyntaxFactory.Tab;
    public static SyntaxTrivia Comment(string text) => (SyntaxTrivia)InternalSyntax.SyntaxFactory.Comment(text);
}

public abstract class StructuredTriviaSyntax : SyntaxNode
{
    protected StructuredTriviaSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0) : base(greenNode, parent, position)
    {
    }
}

public static partial class SyntaxFactory
{
    public static SyntaxTrivia Trivia(StructuredTriviaSyntax structuredTrivia) => (SyntaxTrivia)InternalSyntax.SyntaxFactory.StructuredTrivia((InternalSyntax.SyntaxNode)structuredTrivia.Green);

    public static SkippedTokensTrivia SkippedTokensTrivia(SyntaxTokenList tokens) => new SkippedTokensTrivia(tokens);
}