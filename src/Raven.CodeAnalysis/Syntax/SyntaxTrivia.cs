namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxTrivia
{
    internal readonly InternalSyntax.SyntaxTrivia Green;
    private readonly SyntaxToken? _token;
    private readonly int _position;
    private SyntaxNode? _structure;
    private List<Diagnostic>? _diagnostics;
    private bool? _containsDiagnostics;

    public SyntaxKind Kind => Green.Kind;

    public string Text => Green.Text;

    public SyntaxToken? Token => _token;

    public TextSpan Span => new TextSpan(_position, Green.Width);

    public TextSpan FullSpan => new TextSpan(_position, Green.Width);

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

    public bool ContainsDiagnostics => _containsDiagnostics ??= GetDiagnostics().Any();

    public IEnumerable<Diagnostic> GetDiagnostics()
    {
        foreach (var diagnostic in Green.GetDiagnostics())
        {
            var location = Token?.SyntaxTree!.GetLocation(diagnostic.Span);
            var d = Diagnostic.Create(diagnostic.Descriptor, location, diagnostic.Args);
            (_diagnostics ??= new List<Diagnostic>()).Add(d);
        }

        if (HasStructure)
        {
            var structure = GetStructure()!;

            foreach (var diagnostic in structure.GetDiagnostics())
            {
                (_diagnostics ??= new List<Diagnostic>()).Add(diagnostic);
            }
        }

        return _diagnostics ?? Enumerable.Empty<Diagnostic>();
    }

    public void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitTrivia(this);
    }

    public SyntaxTrivia Accept<TResult>(SyntaxVisitor<TResult> visitor)
    {
        return visitor.VisitTrivia(this);
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
    public static SyntaxTrivia Comment(string text) => (SyntaxTrivia)InternalSyntax.SyntaxFactory.SingleLineComment(text);
}

public abstract class StructuredTriviaSyntax : SyntaxNode
{
    protected StructuredTriviaSyntax(GreenNode greenNode, SyntaxNode? parent, int position = 0) : base(greenNode, parent, position)
    {
    }
}

public static partial class SyntaxFactory
{
    public static SyntaxTrivia Trivia(SyntaxKind kind, string text) => (SyntaxTrivia)InternalSyntax.SyntaxFactory.Trivia(kind, text);

    public static SyntaxTrivia Trivia(StructuredTriviaSyntax structuredTrivia) => (SyntaxTrivia)InternalSyntax.SyntaxFactory.StructuredTrivia((InternalSyntax.SyntaxNode)structuredTrivia.Green);

    public static SkippedTokensTrivia SkippedTokensTrivia(SyntaxTokenList tokens) => new SkippedTokensTrivia(tokens);
}
