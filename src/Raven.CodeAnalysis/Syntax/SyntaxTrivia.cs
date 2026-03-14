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
            return Location.None;
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

    public IEnumerable<SyntaxAnnotation> GetAnnotations(IEnumerable<string> annotationKinds)
    {
        return Green.GetAnnotations(annotationKinds);
    }

    public SyntaxAnnotation? GetAnnotation(string kind)
    {
        return Green.GetAnnotation(kind);
    }

    public bool HasAnnotation(SyntaxAnnotation annotation)
    {
        return Green.HasAnnotation(annotation);
    }

    public bool IsElastic => HasAnnotation(SyntaxAnnotation.ElasticAnnotation);

    public SyntaxTrivia WithAdditionalAnnotations(params SyntaxAnnotation[] annotations)
    {
        return new SyntaxTrivia((InternalSyntax.SyntaxTrivia)Green.WithAdditionalAnnotations(annotations), _token ?? default, _position);
    }

    public override string ToString()
    {
        if (HasStructure)
        {
            return GetStructure()!.ToString();
        }
        return Text;
    }

    public bool ContainsDiagnostics => GetDiagnostics().Any();

    public IEnumerable<Diagnostic> GetDiagnostics()
    {
        var syntaxTree = Token?.SyntaxTree;
        if (syntaxTree is null)
        {
            return Enumerable.Empty<Diagnostic>();
        }

        return syntaxTree.GetDiagnostics(FullSpan);
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
    public static SyntaxTrivia ElasticWhitespace(string text) => Whitespace(text).WithAdditionalAnnotations(SyntaxAnnotation.ElasticAnnotation);
    public static readonly SyntaxTrivia LineFeed = (SyntaxTrivia)InternalSyntax.SyntaxFactory.LineFeed;
    public static readonly SyntaxTrivia ElasticLineFeed = LineFeed.WithAdditionalAnnotations(SyntaxAnnotation.ElasticAnnotation);
    public static readonly SyntaxTrivia CarriageReturn = (SyntaxTrivia)InternalSyntax.SyntaxFactory.CarriageReturn;
    public static readonly SyntaxTrivia ElasticCarriageReturn = CarriageReturn.WithAdditionalAnnotations(SyntaxAnnotation.ElasticAnnotation);
    public static readonly SyntaxTrivia CarriageReturnLineFeed = (SyntaxTrivia)InternalSyntax.SyntaxFactory.CarriageReturnLineFeed;
    public static readonly SyntaxTrivia ElasticCarriageReturnLineFeed = CarriageReturnLineFeed.WithAdditionalAnnotations(SyntaxAnnotation.ElasticAnnotation);
    public static readonly SyntaxTrivia Space = (SyntaxTrivia)InternalSyntax.SyntaxFactory.Space;
    public static readonly SyntaxTrivia ElasticSpace = Space.WithAdditionalAnnotations(SyntaxAnnotation.ElasticAnnotation);
    public static readonly SyntaxTrivia Tab = (SyntaxTrivia)InternalSyntax.SyntaxFactory.Tab;
    public static readonly SyntaxTrivia ElasticTab = Tab.WithAdditionalAnnotations(SyntaxAnnotation.ElasticAnnotation);
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
