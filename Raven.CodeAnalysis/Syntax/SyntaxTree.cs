using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Text;

using Raven.CodeAnalysis.Parser.Internal;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax;

public class SyntaxTree
{
    private CompilationUnitSyntax _compilationUnit;
    private SourceText? _sourceText;
    private readonly ParseOptions _options;
    private readonly DiagnosticBag _diagnosticBag;
    private readonly Encoding _encoding;

    public Encoding Encoding => _sourceText?.Encoding ?? _encoding;

    private SyntaxTree(ParseOptions options, Encoding? encoding)
    {
        _encoding = encoding ?? Encoding.UTF8;
        _options = options ?? new ParseOptions();
        _diagnosticBag = new DiagnosticBag();
    }

    private SyntaxTree(SourceText sourceText, ParseOptions? options, DiagnosticBag diagnosticBag)
    {
        _sourceText = sourceText;
        _options = options ?? new ParseOptions();
        _diagnosticBag = diagnosticBag;
    }

    public CompilationUnitSyntax GetRoot() { return _compilationUnit; }

    public static SyntaxTree ParseText(string text, ParseOptions? options = null, Encoding? encoding = null)
    {
        var sourceText = SourceText.From(text, encoding);

        return ParseText(sourceText, options);
    }

    public static SyntaxTree ParseText(SourceText sourceText, ParseOptions? options = null)
    {
        DiagnosticBag diagnosticBag = new();

        var parser = new Parser.SyntaxParser(options ?? new ParseOptions(), diagnosticBag);

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
        return oldTree.GetText().GetTextChanges(this.GetText());
    }

    public static SyntaxTree Create(CompilationUnitSyntax compilationUnit, ParseOptions? options = null, Encoding? encoding = null)
    {
        var syntaxTree = new SyntaxTree(options, encoding);

        compilationUnit = compilationUnit
            .WithRoot(syntaxTree);

        syntaxTree.AddSyntaxTree(compilationUnit);

        return syntaxTree;
    }


    internal static SyntaxTree Create(SourceText sourceText, CompilationUnitSyntax compilationUnit, ParseOptions options, DiagnosticBag diagnosticBag)
    {
        var syntaxTree = new SyntaxTree(sourceText, options, diagnosticBag);

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
            _sourceText = SourceText.From(GetRoot().ToFullString());
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

        var matchingNodes = root.DescendantNodes()
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
        var oldText = _sourceText;

        var changes = newText.GetTextChanges(oldText);

        if (changes.Count == 0)
            return this;

        var root = GetRoot();

        CompilationUnitSyntax newCompilationUnit = root;

        foreach (var change in changes)
        {
            var changedNode = GetNodeToReplace(change.Span);

            if (changedNode is null)
                continue;

            var diagnosticBag = new DiagnosticBag();

            SyntaxNode? newNode = ParseNodeFromText(change.Span, newText, changedNode, diagnosticBag);

            newCompilationUnit = (CompilationUnitSyntax)newCompilationUnit
                .ReplaceNode(changedNode, newNode);
        }

        return SyntaxTree.Create(newText, newCompilationUnit, _options, _diagnosticBag);
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

        var parser = new Parser.SyntaxParser(_options, diagnosticBag);

        return parser.ParseSyntax(requestedSyntaxType, newText, position);
    }
}

public static partial class SyntaxFactory
{
    public static SyntaxTree ParseSyntaxTree(SourceText sourceText, ParseOptions? options = null) => Syntax.SyntaxTree.ParseText(sourceText, options);

    public static SyntaxTree ParseSyntaxTree(string text, ParseOptions? options = null, Encoding? encoding = null) => Syntax.SyntaxTree.ParseText(text, options, encoding);

    //public static SyntaxTree SyntaxTree(CompilationUnitSyntax root, ParseOptions? options = default, string path = "", Encoding? encoding = default)
    //    => Syntax.SyntaxTree.Create(root, options, encoding);
}