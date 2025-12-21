using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Documentation;

internal static class DocumentationCommentValidator
{
    public static void Analyze(SemanticModel semanticModel, SyntaxNode root, DiagnosticBag diagnostics)
    {
        var options = semanticModel.SyntaxTree.Options ?? new ParseOptions();

        if (!options.DocumentationMode)
            return;

        var associatedTrivia = new HashSet<SyntaxTrivia>();
        var fallbackLocation = Location.None;

        foreach (var node in root.DescendantNodes())
        {
            var format = node.SyntaxTree?.Options?.DocumentationFormat ?? options.DocumentationFormat;

            if (!DocumentationCommentUtilities.TryGetDocumentationComment(node, format, out var comment, out var trivia))
                continue;

            foreach (var item in trivia)
                associatedTrivia.Add(item);

            if (comment is null)
                continue;

            var location = GetDocumentationLocation(node, trivia) ?? fallbackLocation;
            ValidateContent(format, comment.Content, location, diagnostics);
        }

        foreach (var trivia in root.DescendantTrivia())
        {
            if (!IsDocumentationTrivia(trivia.Kind))
                continue;

            if (associatedTrivia.Contains(trivia))
                continue;

            diagnostics.ReportDocumentationCommentNotAttached(trivia.GetLocation());
        }
    }

    private static void ValidateContent(DocumentationFormat format, string content, Location location, DiagnosticBag diagnostics)
    {
        switch (format)
        {
            case DocumentationFormat.Xml:
                if (!TryValidateXml(content, out var xmlError))
                    diagnostics.ReportDocumentationCommentInvalidXml(xmlError, location);
                break;
            case DocumentationFormat.Markdown:
            default:
                if (!TryValidateMarkdown(content, out var markdownError))
                    diagnostics.ReportDocumentationCommentInvalidMarkdown(markdownError, location);
                break;
        }
    }

    private static bool TryValidateXml(string content, out string message)
    {
        try
        {
            _ = XDocument.Parse($"<doc>{content}</doc>");
            message = string.Empty;
            return true;
        }
        catch (XmlException ex)
        {
            message = ex.Message;
            return false;
        }
    }

    private static bool TryValidateMarkdown(string content, out string message)
    {
        // Look for unterminated fenced code blocks (```), a common authoring error.
        var fenceCount = CountOccurrences(content, "```");

        if (fenceCount % 2 != 0)
        {
            message = "Unterminated fenced code block";
            return false;
        }

        message = string.Empty;
        return true;
    }

    private static int CountOccurrences(string content, string value)
    {
        if (string.IsNullOrEmpty(content) || string.IsNullOrEmpty(value))
            return 0;

        var count = 0;
        var index = 0;

        while ((index = content.IndexOf(value, index, StringComparison.Ordinal)) >= 0)
        {
            count++;
            index += value.Length;
        }

        return count;
    }

    private static bool IsDocumentationTrivia(SyntaxKind kind)
    {
        return kind is SyntaxKind.SingleLineDocumentationCommentTrivia or SyntaxKind.MultiLineDocumentationCommentTrivia;
    }

    private static Location? GetDocumentationLocation(SyntaxNode node, IReadOnlyList<SyntaxTrivia> trivia)
    {
        if (trivia.Count > 0)
            return trivia[0].GetLocation();

        return node.GetLocation();
    }
}
