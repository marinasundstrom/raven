using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Parser.Internal;
using Raven.CodeAnalysis.Syntax;

[assembly: InternalsVisibleTo("Test")]
[assembly: InternalsVisibleTo("Raven.Tests")]

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

    public CompilationUnitSyntax GetSyntaxRoot() { return _compilationUnit; }

    public static SyntaxTree ParseText(string text)
    {
        var sourceText = new SourceText(text);

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
        throw new NotImplementedException(); //_diagnosticBag.ToImmutableArray();
    }

    public object GetChanges(SyntaxTree syntaxTree)
    {
        throw new NotImplementedException();
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
            text = new SourceText("");
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
        var root = GetSyntaxRoot();

        // Find the node whose span matches the given TextSpan
        var matchingNode = root.DescendantNodes()
            .Where(node => node.FullSpan.Contains(span))
            .OrderBy(node => node.FullSpan.Length); // Sort by span length to get the shortest;

        return matchingNode.FirstOrDefault();
    }
    
    public SyntaxTree WithUpdatedText(SourceText sourceText, int changeStart, int changeEnd)
    {
        /*
        var updatedNodes = new List<SyntaxNode>();

        foreach (var node in _nodes)
        {
            // Check if the node overlaps with the change
            if (node.End <= changeStart || node.Start >= changeEnd)
            {
                // Unchanged node
                updatedNodes.Add(node);
            }
            else
            {
                // Re-parse only the affected node
                string updatedText = sourceText.Text.Substring(node.Start, node.End - node.Start);
                updatedNodes.Add(new SyntaxNode(updatedText, node.Start));
            }
        }
        */

        return default!; // new SyntaxTree(updatedNodes);
    }
}