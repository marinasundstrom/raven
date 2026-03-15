using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>Represents a context-driven code action that is not tied to a diagnostic.</summary>
public sealed record CodeRefactoring(
    DocumentId DocumentId,
    TextSpan Span,
    CodeAction Action,
    CodeRefactoringProvider Provider);
