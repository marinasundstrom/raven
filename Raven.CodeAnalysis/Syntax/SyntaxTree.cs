using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Parser.Internal;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax;

public class SyntaxTree
{
    private CompilationUnitSyntax _compilationUnit;
    private SourceText? _sourceText;
    private DiagnosticBag _diagnosticBag;

    private SyntaxTree()
    {
        _diagnosticBag = new DiagnosticBag();
    }

    private SyntaxTree(DiagnosticBag diagnosticBag)
    : this(null, diagnosticBag)
    {

    }

    private SyntaxTree(SourceText sourceText, DiagnosticBag diagnosticBag)
    {
        _sourceText = sourceText;
        _diagnosticBag = diagnosticBag;
    }

    private SyntaxTree(SourceText sourceText)
        : this(sourceText, new DiagnosticBag())
    {

    }

    public CompilationUnitSyntax GetRoot() { return _compilationUnit; }

    public static SyntaxTree ParseText(string text)
    {
        var sourceText = SourceText.From(text);

        DiagnosticBag diagnosticBag = new DiagnosticBag();

        var parser = new Parser.SyntaxParser(diagnosticBag);

        return parser.Parse(sourceText);
    }

    public IEnumerable<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        return _diagnosticBag.ToImmutableArray();
    }

    public IEnumerable<Diagnostic> GetDiagnostics(SyntaxNodeOrToken syntaxNodeOrToken)
    {
        TextSpan span = syntaxNodeOrToken.Node?.Span ?? syntaxNodeOrToken.Token.Span;

        return GetDiagnostics().Where(x => x.Location.Span.Contains(span));
    }

    public IEnumerable<TextChange> GetChanges(SyntaxTree oldTree)
    {
        return oldTree.GetText().GetChangeRanges(this.GetText());
    }

    public static SyntaxTree Create(CompilationUnitSyntax compilationUnit)
    {
        var syntaxTree = new SyntaxTree();

        compilationUnit = compilationUnit
            .WithRoot(syntaxTree);

        syntaxTree.AddSyntaxTree(compilationUnit);

        return syntaxTree;
    }


    internal static SyntaxTree Create(SourceText sourceText, CompilationUnitSyntax compilationUnit, DiagnosticBag diagnosticBag)
    {
        var syntaxTree = new SyntaxTree(sourceText, diagnosticBag);

        compilationUnit = compilationUnit
            .WithRoot(syntaxTree);

        syntaxTree.AddSyntaxTree(compilationUnit);

        return syntaxTree;
    }

    private void AddSyntaxTree(CompilationUnitSyntax compilationUnit)
    {
        _compilationUnit = compilationUnit;
    }

    public Location GetLocation(TextSpan span)
    {
        var sourceText = GetText();

        if (sourceText is null)
            throw new Exception();

        var (line, column) = sourceText.GetLineAndColumn(span);
        return new Location(line, column);
    }

    public SourceText? GetText()
    {
        return _sourceText;
    }

    public bool TryGetText([NotNullWhen(true)] out SourceText? text)
    {
        if (_sourceText is not null)
        {
            text = SourceText.From("");
            return true;
        }
        text = null;
        return false;
    }

    public SyntaxNode? GetNodeForSpan(TextSpan span)
    {
        // Ensure the SyntaxTree corresponds to the SourceText
        if (!this.TryGetText(out var syntaxTreeText))
            throw new ArgumentException("SourceText does not match the provided SyntaxTree.");

        // Get the root node of the syntax tree
        var root = GetRoot();

        // Find the node whose span matches the given TextSpan
        var matchingNode = root.DescendantNodes()
            .Where(node => node.FullSpan.Contains(span))
            .OrderBy(node => node.FullSpan.Length); // Sort by span length to get the shortest;

        return matchingNode.FirstOrDefault();
    }
}