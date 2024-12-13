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

        return ParseText(sourceText);
    }
    
    public static SyntaxTree ParseText(SourceText sourceText)
    {
        DiagnosticBag diagnosticBag = new();

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
            text = _sourceText;
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

    public SyntaxTree WithChangedText(SourceText newText)
    {
        var oldText = _sourceText;
        
        var changes = newText.GetChangeRanges(oldText);

        if (changes.Count == 0)
            return this;

        var root = GetRoot();

        CompilationUnitSyntax newCompilationUnit = root;
        
        foreach (var change in changes)
        {
            var changedNode = GetNodeForSpan(change.Span);

            if (changedNode is null)
                continue;

            var changedText = oldText.GetSubText(change.Span);

            var newText2 = change.NewText;

            var diagnosticBag = new DiagnosticBag();
                
            SyntaxNode? newNode = ParseText(newText2, diagnosticBag);

            newCompilationUnit = (CompilationUnitSyntax)newCompilationUnit
                .ReplaceNode(changedNode, newNode);
        }

        return SyntaxTree.Create(newText, newCompilationUnit, _diagnosticBag);
    }

    private SyntaxNode? ParseText(string changedText, DiagnosticBag diagnosticBag)
    {
        return SyntaxFactory.IdentifierName("FooBar");
    }
}