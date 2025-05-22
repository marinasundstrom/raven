using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Represents a single code completion suggestion.
/// </summary>
public record CompletionItem(
    string DisplayText,                     // What is shown in the completion list
    string InsertionText,                   // What is inserted into the document
    TextSpan ReplacementSpan,              // The span of text to replace (can be empty if just insertion)
    int? CursorOffset = null,              // Cursor position relative to start of InsertionText (null = end)
    string? Description = null             // Optional description (tooltip or documentation)
);