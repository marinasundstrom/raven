using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Text;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax;

public class SyntaxTree
{
    private CompilationUnitSyntax _compilationUnit;
    private SourceText _sourceText;
    private readonly ParseOptions _options;
    private DiagnosticBag? _diagnosticBag = new DiagnosticBag();

    internal SyntaxTree(SourceText sourceText, string filePath, ParseOptions? options)
    {
        _sourceText = sourceText;
        FilePath = filePath ?? "file";
        _options = options ?? new ParseOptions();
    }

    public Encoding Encoding => _sourceText.Encoding;
    public string FilePath { get; }
    public bool HasCompilationUnit => _compilationUnit is not null;
    public int Length => _sourceText.Length;
    public ParseOptions Options { get; }

    public CompilationUnitSyntax GetRoot(CancellationToken cancellationToken = default) { return _compilationUnit; }

    public static SyntaxTree ParseText(string text, ParseOptions? options = null, Encoding? encoding = null, string? path = null)
    {
        var sourceText = SourceText.From(text, encoding);

        return ParseText(sourceText, options);
    }

    public static SyntaxTree ParseText(SourceText sourceText, ParseOptions? options = null, string? path = null)
    {
        var parser = new Parser.SyntaxParser(path ?? "file", options ?? new ParseOptions());

        return parser.Parse(sourceText);
    }

    public IEnumerable<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        return _diagnosticBag.ToImmutableArray();
    }

    public IEnumerable<Diagnostic> GetDiagnostics(SyntaxNodeOrToken syntaxNodeOrToken)
    {
        TextSpan span = syntaxNodeOrToken.Node?.FullSpan ?? syntaxNodeOrToken.Token.FullSpan;

        return GetDiagnostics().Where(x => x.Location.SourceSpan.Contains(span));
    }

    public IEnumerable<TextChange> GetChanges(SyntaxTree oldTree)
    {
        return oldTree.GetText().GetTextChanges(this.GetText());
    }

    public static SyntaxTree Create(CompilationUnitSyntax compilationUnit, ParseOptions? options = null, Encoding? encoding = null, string? filePath = null)
    {
        var sourceText = SourceText.From(compilationUnit.ToFullString(), encoding);

        var syntaxTree = new SyntaxTree(sourceText, filePath, options);

        compilationUnit = compilationUnit
            .WithSyntaxTree(syntaxTree);

        syntaxTree.AttachSyntaxRoot(compilationUnit);

        return syntaxTree;
    }


    internal static SyntaxTree Create(SourceText sourceText, CompilationUnitSyntax compilationUnit, ParseOptions options)
    {
        var syntaxTree = new SyntaxTree(sourceText, string.Empty, options);

        compilationUnit = compilationUnit
            .WithSyntaxTree(syntaxTree);

        syntaxTree.AttachSyntaxRoot(compilationUnit);

        return syntaxTree;
    }

    internal void AttachSyntaxRoot(CompilationUnitSyntax compilationUnit)
    {
        _compilationUnit = compilationUnit;
    }

    public Location GetLocation(TextSpan span)
    {
        var sourceText = GetText();

        if (sourceText is null)
            throw new Exception();

        var (line, col) = sourceText.GetLineAndColumn(span);

        return Location.Create(this, span);
    }

    public SourceText? GetText()
    {
        if (_sourceText is null)
        {
            _sourceText = SourceText.From(GetRoot().ToFullString());
        }
        return _sourceText;
    }

    public bool TryGetText([NotNullWhen(true)] out SourceText? text)
    {
        if (_sourceText is not null)
        {
            text = _sourceText;
            return true;
        }
        if (_sourceText is null)
        {
            text = _sourceText = SourceText.From(GetRoot().ToFullString());
            return true;
        }
        text = null;
        return false;
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

    public SyntaxTree WithChangedText(SourceText newText)
    {
        var oldText = GetText();

        var changes = newText.GetTextChanges(oldText);

        if (changes.Count == 0)
            return this;

        var root = GetRoot();

        CompilationUnitSyntax newCompilationUnit = root;

        bool reparse = false;

        foreach (var change in changes)
        {
            var changedNode = GetNodeToReplace(change.Span);

            if (changedNode is null)
                continue;

            var diagnosticBag = new DiagnosticBag();

            SyntaxNode? newNode = ParseNodeFromText(change.Span, newText, changedNode, diagnosticBag);

            if (newNode is null)
            {
                // Failed to resolve target syntax type
                reparse = true;
                break;
            }

            newCompilationUnit = (CompilationUnitSyntax)newCompilationUnit
                .ReplaceNode(changedNode, newNode);
        }

        if (reparse)
        {
            // Fallback: Reparse the entire tree
            return SyntaxTree.ParseText(newText, _options, FilePath);
        }

        return SyntaxTree.Create(newCompilationUnit, _options, Encoding, FilePath);
    }

    private SyntaxNode? ParseNodeFromText(TextSpan changeSpan, SourceText newText, SyntaxNode nodeToReplace, DiagnosticBag diagnosticBag)
    {
        Type requestedSyntaxType;

        if (changeSpan.Length == 0)
        {
            requestedSyntaxType = nodeToReplace.GetType();
        }
        else
        {
            var parent = nodeToReplace.Parent;

            if (parent is BlockSyntax block)
            {
                //block.ReplaceNode(nodeToReplace, );

                requestedSyntaxType = typeof(StatementSyntax);
            }
            else
            {
                requestedSyntaxType = parent.GetPropertyTypeForChild(nodeToReplace)!;
            }
        }

        var position = nodeToReplace.FullSpan.Start;

        var parser = new Parser.SyntaxParser(string.Empty, _options);

        return parser.ParseSyntax(requestedSyntaxType, newText, position);
    }

    internal void AddDiagnostics(DiagnosticBag diagnostics)
    {
        _diagnosticBag = diagnostics;
    }
}

public static partial class SyntaxFactory
{
    public static SyntaxTree ParseSyntaxTree(SourceText sourceText, ParseOptions? options = null, string? filePath = null) => Syntax.SyntaxTree.ParseText(sourceText, options, filePath);

    public static SyntaxTree ParseSyntaxTree(string text, ParseOptions? options = null, Encoding? encoding = null, string? filePath = null) => Syntax.SyntaxTree.ParseText(text, options, encoding, filePath);

    //public static SyntaxTree SyntaxTree(CompilationUnitSyntax root, ParseOptions? options = default, string path = "", Encoding? encoding = default)
    //    => Syntax.SyntaxTree.Create(root, options, encoding);
}