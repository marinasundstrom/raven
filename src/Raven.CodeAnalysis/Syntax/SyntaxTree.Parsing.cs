using System.Text;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax;

internal enum IncrementalParseFallbackReason
{
    None,
    ConditionalDirectives,
    PreviousFallback,
    ExistingRecoverySyntax,
    ChangePolicy,
    NodeParseFailure,
    NewRecoverySyntax,
    ReconstructedTextMismatch
}

internal sealed class IncrementalParseFallbackException : InvalidOperationException
{
    internal IncrementalParseFallbackException(
        IncrementalParseFallbackReason reason,
        string filePath)
        : base($"Incremental parsing required a full-document fallback for '{filePath}': {reason}.")
    {
        Reason = reason;
    }

    internal IncrementalParseFallbackReason Reason { get; }
}

public partial class SyntaxTree
{
    internal const int IncrementalParseMaxChangeLength = 4096;

    internal IncrementalParseFallbackReason IncrementalParseFallbackReason { get; private set; }

    public static SyntaxTree ParseText(string text, ParseOptions? options = null, Encoding? encoding = null, string? path = null)
    {
        var sourceText = SourceText.From(text, encoding);

        return ParseText(sourceText, options, path);
    }

    public static SyntaxTree ParseText(SourceText sourceText, ParseOptions? options = null, string? path = null)
        => ParseTextCore(sourceText, options, path, isGenerated: false);

    internal static SyntaxTree ParseGeneratedText(SourceText sourceText, string path)
        => ParseTextCore(sourceText, options: null, path, isGenerated: true);

    private static SyntaxTree ParseTextCore(SourceText sourceText, ParseOptions? options, string? path, bool isGenerated)
    {
        var parser = new InternalSyntax.Parser.LanguageParser(path ?? "file", options ?? new ParseOptions());

        var parseResult = parser.Parse(sourceText);
        var compilationUnit = (CompilationUnitSyntax)parseResult.Root.CreateRed();

        var sourceTree = new SyntaxTree(sourceText, path ?? "file", options, isGenerated);

        compilationUnit = compilationUnit
            .WithSyntaxTree(sourceTree);

        sourceTree.AttachSyntaxRoot(compilationUnit);
        sourceTree.AttachDiagnostics(parseResult.Diagnostics);

        return sourceTree;
    }

    public SyntaxTree WithChangedText(SourceText newText)
    {
        var oldText = GetText();

        var changeRanges = newText.GetChangeRanges(oldText);

        if (changeRanges.Count == 0)
            return this;

        var root = GetRoot();

        // A fallback tree is correct but does not carry incremental identity
        // guarantees for the malformed region. Give the next edit one clean
        // full parse before resuming incremental replacement. PreviousFallback
        // is deliberately not sticky, so recovery does not disable incremental
        // parsing for all later edits.
        if (IncrementalParseFallbackReason is not IncrementalParseFallbackReason.None and
            not IncrementalParseFallbackReason.PreviousFallback)
        {
            return ParseTextWithFallback(newText, IncrementalParseFallbackReason.PreviousFallback);
        }

        if (ContainsConditionalDirectives(oldText) || ContainsConditionalDirectives(newText))
            return ParseTextWithFallback(newText, IncrementalParseFallbackReason.ConditionalDirectives);

        if (ShouldFullyReparseChangedText(oldText, newText, changeRanges))
            return ParseTextWithFallback(newText, IncrementalParseFallbackReason.ChangePolicy);

        var changes = newText.GetTextChanges(oldText);

        CompilationUnitSyntax newCompilationUnit = root;
        var updatedDiagnostics = GetDiagnostics()
            .Select(static diagnostic => InternalSyntax.DiagnosticInfo.Create(
                diagnostic.Descriptor,
                diagnostic.Location.SourceSpan,
                diagnostic.GetMessageArgs()))
            .ToList();

        var fallbackReason = IncrementalParseFallbackReason.None;

        foreach (var change in changes)
        {
            var changedNode = GetNodeToReplace(change.Span);

            if (changedNode is null)
                continue;

            // Recovery nodes can own text outside the construct that originally
            // triggered them. Only edits whose replacement region contains recovery
            // syntax need the conservative full-document parse; unrelated malformed
            // siblings must not defeat incremental identity reuse.
            if (ContainsRecoverySyntax(changedNode))
            {
                fallbackReason = IncrementalParseFallbackReason.ExistingRecoverySyntax;
                break;
            }

            var parseResult = ParseNodeFromText(change.Span, newText, changedNode);

            if (parseResult is null)
            {
                // Failed to resolve target syntax type
                fallbackReason = IncrementalParseFallbackReason.NodeParseFailure;
                break;
            }

            if (ContainsRecoverySyntax(parseResult.Value.Node))
            {
                fallbackReason = IncrementalParseFallbackReason.NewRecoverySyntax;
                break;
            }

            newCompilationUnit = newCompilationUnit
                .ReplaceNode(parseResult.Value.ReplacedNode, parseResult.Value.Node);

            updatedDiagnostics = UpdateDiagnostics(
                updatedDiagnostics,
                parseResult.Value.ReplacedNode.FullSpan,
                change,
                parseResult.Value.Diagnostics);
        }

        if (fallbackReason != IncrementalParseFallbackReason.None)
        {
            // Fallback: Reparse the entire tree
            return ParseTextWithFallback(newText, fallbackReason);
        }

        var updatedTree = Create(
            newText,
            newCompilationUnit,
            _options,
            FilePath,
            updatedDiagnostics.OrderBy(static diagnostic => diagnostic.Span.Start), isGenerated: IsGenerated);
        if (!string.Equals(updatedTree.GetRoot().ToFullString(), newText.ToString(), StringComparison.Ordinal))
        {
            return ParseTextWithFallback(newText, IncrementalParseFallbackReason.ReconstructedTextMismatch);
        }

        return updatedTree;
    }

    private static bool ContainsRecoverySyntax(SyntaxNode node)
        => node.DescendantNodesAndSelf().Any(static descendant => descendant.IsMissing) ||
           node.DescendantTokens().Any(static token => token.IsMissing) ||
           node.DescendantTrivia().Any(static trivia => trivia.Kind == SyntaxKind.SkippedTokensTrivia);

    private SyntaxTree ParseTextWithFallback(SourceText newText, IncrementalParseFallbackReason reason)
    {
        if (_options.ThrowOnIncrementalParseFallback)
            throw new IncrementalParseFallbackException(reason, FilePath);

        var tree = ParseTextCore(newText, _options, FilePath, IsGenerated);
        tree.IncrementalParseFallbackReason = reason;
        return tree;
    }

    private static List<InternalSyntax.DiagnosticInfo> UpdateDiagnostics(
        IEnumerable<InternalSyntax.DiagnosticInfo> existingDiagnostics,
        TextSpan replacedSpan,
        TextChange change,
        IEnumerable<InternalSyntax.DiagnosticInfo> replacementDiagnostics)
    {
        var delta = change.NewText.Length - change.Span.Length;
        var diagnostics = new List<InternalSyntax.DiagnosticInfo>();

        foreach (var diagnostic in existingDiagnostics)
        {
            if (IsOwnedByReplacedSpan(diagnostic.Span, replacedSpan))
                continue;

            var span = diagnostic.Span;
            if (span.Start >= change.Span.End)
            {
                span = new TextSpan(span.Start + delta, span.Length);
            }

            diagnostics.Add(InternalSyntax.DiagnosticInfo.Create(
                diagnostic.Descriptor,
                span,
                diagnostic.Args));
        }

        diagnostics.AddRange(replacementDiagnostics);
        return diagnostics;
    }

    private static bool IsOwnedByReplacedSpan(TextSpan diagnosticSpan, TextSpan replacedSpan)
    {
        if (diagnosticSpan.Length != 0)
            return diagnosticSpan.IntersectsWith(replacedSpan);

        return diagnosticSpan.Start >= replacedSpan.Start &&
               diagnosticSpan.Start < replacedSpan.End;
    }

    private static bool ContainsConditionalDirectives(SourceText text)
    {
        using var reader = text.GetTextReader();
        string? line;
        while ((line = reader.ReadLine()) is not null)
        {
            var trimmed = line.AsSpan().TrimStart();
            if (trimmed.StartsWith("#if", StringComparison.Ordinal) ||
                trimmed.StartsWith("#elif", StringComparison.Ordinal) ||
                trimmed.StartsWith("#else", StringComparison.Ordinal) ||
                trimmed.StartsWith("#endif", StringComparison.Ordinal))
            {
                return true;
            }
        }

        return false;
    }

    internal static bool ShouldFullyReparseChangedText(
        SourceText oldText,
        SourceText newText,
        IReadOnlyList<TextChangeRange> changeRanges)
    {
        if (changeRanges.Count != 1)
            return true;

        var change = changeRanges[0];
        if (change.Span.Start == 0 &&
            change.Span.Length == oldText.Length &&
            change.NewLength == newText.Length)
        {
            return true;
        }

        return change.Span.Length > IncrementalParseMaxChangeLength ||
               change.NewLength > IncrementalParseMaxChangeLength;
    }

    private IncrementalParseResult? ParseNodeFromText(TextSpan changeSpan, SourceText newText, SyntaxNode nodeToReplace)
    {
        for (var candidate = nodeToReplace; candidate.Parent is not null; candidate = candidate.Parent)
        {
            Type requestedSyntaxType;
            var parent = candidate.Parent;

            if (changeSpan.Length == 0 && ReferenceEquals(candidate, nodeToReplace))
            {
                requestedSyntaxType = candidate.GetType();
            }
            else if (parent is TypeDeclarationSyntax && candidate is MemberDeclarationSyntax)
            {
                // A standalone MemberDeclaration parse has compilation-unit context and may
                // classify a method-shaped declaration as a global statement. Preserve the
                // concrete member category while reparsing inside a type. If an edit truly
                // changes the declaration category, the type check below triggers the safe
                // full-tree fallback.
                requestedSyntaxType = candidate.GetType();
            }
            else if (parent is BlockStatementSyntax)
            {
                requestedSyntaxType = typeof(StatementSyntax);
            }
            else
            {
                var childType = parent.GetPropertyTypeForChild(candidate);
                if (childType is null)
                    continue;

                requestedSyntaxType = childType;
            }

            var parser = new InternalSyntax.Parser.LanguageParser(string.Empty, _options);
            var parseResult = parser.ParseSyntaxWithDiagnostics(
                requestedSyntaxType,
                newText,
                candidate.FullSpan.Start);
            if (parseResult is null)
                continue;

            var newNode = parseResult.Value.Root.CreateRed();
            if (!requestedSyntaxType.IsInstanceOfType(newNode) || newNode.IsMissing)
                continue;

            return new IncrementalParseResult(candidate, newNode, parseResult.Value.Diagnostics);
        }

        return null;
    }

    private readonly record struct IncrementalParseResult(
        SyntaxNode ReplacedNode,
        SyntaxNode Node,
        IReadOnlyList<InternalSyntax.DiagnosticInfo> Diagnostics);
}

public static partial class SyntaxFactory
{
    public static SyntaxTree ParseSyntaxTree(SourceText sourceText, ParseOptions? options = null, string? filePath = null) => Syntax.SyntaxTree.ParseText(sourceText, options, filePath);

    public static SyntaxTree ParseSyntaxTree(string text, ParseOptions? options = null, Encoding? encoding = null, string? filePath = null) => Syntax.SyntaxTree.ParseText(text, options, encoding, filePath);

    //public static SyntaxTree SyntaxTree(CompilationUnitSyntax root, ParseOptions? options = default, string path = "", Encoding? encoding = default)
    //    => Syntax.SyntaxTree.Create(root, options, encoding);
}
