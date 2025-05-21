namespace Raven.CodeAnalysis;

public record CompletionItem(string DisplayText, string InsertionText, string? Description = null);
