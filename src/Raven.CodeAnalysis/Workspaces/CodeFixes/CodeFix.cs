namespace Raven.CodeAnalysis;

/// <summary>Represents a code action associated with a specific diagnostic.</summary>
public sealed record CodeFix(
    DocumentId DocumentId,
    Diagnostic Diagnostic,
    CodeAction Action,
    CodeFixProvider Provider);

/// <summary>Result of applying code fixes in a workspace.</summary>
public readonly record struct ApplyCodeFixesResult(Solution Solution, int AppliedFixCount);
