using System.Diagnostics.CodeAnalysis;
using System.Text;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax;

public partial class SyntaxTree
{
    private CompilationUnitSyntax? _compilationUnit;
    private readonly SourceText _sourceText;
    private readonly ParseOptions _options;
    private IReadOnlyList<Diagnostic>? _diagnostics;

    internal SyntaxTree(SourceText sourceText, string filePath, ParseOptions? options, bool isGenerated = false)
    {
        IsGenerated = isGenerated;
        _sourceText = sourceText;
        FilePath = filePath ?? "file";
        _options = (options ?? new ParseOptions()).Snapshot();
    }

    internal bool IsGenerated { get; }

    public Encoding Encoding => _sourceText.Encoding;
    public string FilePath { get; }
    public bool HasCompilationUnit => _compilationUnit is not null;
    public int Length => _sourceText.Length;
    public ParseOptions Options => _options;

    public CompilationUnitSyntax GetRoot(CancellationToken cancellationToken = default) =>
        _compilationUnit ?? throw new InvalidOperationException("The syntax root has not been attached.");

    public IEnumerable<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        return _diagnostics ?? Enumerable.Empty<Diagnostic>();
    }

    public IEnumerable<Diagnostic> GetDiagnostics(SyntaxNodeOrToken syntaxNodeOrToken)
    {
        if (_diagnostics is null)
        {
            return Enumerable.Empty<Diagnostic>();
        }

        var span = syntaxNodeOrToken.IsNode
            ? syntaxNodeOrToken.AsNode()!.FullSpan
            : syntaxNodeOrToken.AsToken().FullSpan;

        return _diagnostics.Where(d => d.Location.SourceSpan.IntersectsWith(span));
    }

    public IEnumerable<Diagnostic> GetDiagnostics(TextSpan span)
    {
        if (_diagnostics is null)
        {
            return Enumerable.Empty<Diagnostic>();
        }

        return _diagnostics.Where(d => d.Location.SourceSpan.IntersectsWith(span));
    }

    public IEnumerable<TextChange> GetChanges(SyntaxTree oldTree)
    {
        return GetText().GetTextChanges(oldTree.GetText());
    }

    public static SyntaxTree Create(CompilationUnitSyntax compilationUnit, ParseOptions? options = null, Encoding? encoding = null, string? filePath = null)
    {
        var sourceText = SourceText.From(compilationUnit.ToFullString(), encoding);

        var syntaxTree = new SyntaxTree(sourceText, filePath ?? "file", options);

        var root = compilationUnit.WithSyntaxTree(syntaxTree);

        syntaxTree.AttachSyntaxRoot(root);
        syntaxTree.AttachDiagnostics(Array.Empty<InternalSyntax.DiagnosticInfo>());

        return syntaxTree;
    }


    internal static SyntaxTree Create(
        SourceText sourceText,
        CompilationUnitSyntax compilationUnit,
        ParseOptions options,
        string? filePath = null,
        IEnumerable<InternalSyntax.DiagnosticInfo>? diagnostics = null,
        bool isGenerated = false)
    {
        var syntaxTree = new SyntaxTree(sourceText, filePath ?? string.Empty, options, isGenerated);

        var root = compilationUnit.WithSyntaxTree(syntaxTree);

        syntaxTree.AttachSyntaxRoot(root);
        syntaxTree.AttachDiagnostics(diagnostics ?? Array.Empty<InternalSyntax.DiagnosticInfo>());

        return syntaxTree;
    }

    internal void AttachSyntaxRoot(CompilationUnitSyntax compilationUnit)
    {
        _compilationUnit = compilationUnit;
    }

    internal void AttachDiagnostics(IEnumerable<InternalSyntax.DiagnosticInfo> diagnostics)
    {
        _diagnostics = diagnostics
            .Select(d => Diagnostic.Create(d.Descriptor, GetLocation(d.Span), d.Args))
            .ToArray();
    }

    public Location GetLocation(TextSpan span)
    {
        var sourceText = GetText();

        var (line, col) = sourceText.GetLineAndColumn(span);

        return Location.Create(this, span);
    }

    public SourceText GetText() => _sourceText;

    public bool TryGetText([NotNullWhen(true)] out SourceText? text)
    {
        text = _sourceText;
        return true;
    }

    /// <summary>
    /// Gets the nodes in span.
    /// </summary>
    /// <param name="span"></param>
    /// <returns>An enumerable of nodes that return the the innermost node first</returns>
    public IEnumerable<SyntaxNode> GetNodesInSpan(TextSpan span)
    {
        // Ensure the SyntaxTree corresponds to the SourceText
        if (!this.TryGetText(out var syntaxTreeText))
            throw new ArgumentException("SourceText does not match the provided SyntaxTree.");

        // Get the root node of the syntax tree
        var root = GetRoot();

        // Find the nodes whose span matches the given TextSpan

        var matchingNodes = root.DescendantNodesAndSelf()
            .Where(node => node.Span.Contains(span))
            .Reverse();

        return matchingNodes;
    }

    public SyntaxNode? GetNodeToReplace(TextSpan span)
    {
        var matchingNodes = GetNodesInSpan(span);
        var node = matchingNodes.FirstOrDefault();
        if (span.Length == 0)
        {
            // TEMPORARY:
            // If the length of "span" is 0, then something has been added to the tree.
            // We should get the parent node of the innermost instead.
            node = node?.Parent;
        }
        return node;
    }


    public SyntaxNode? GetNodeForSpan(TextSpan span)
    {
        // Get the first node whose span matches the given TextSpan

        var matchingNodes = GetNodesInSpan(span);
        return matchingNodes.FirstOrDefault();
    }

}
