using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>Context supplied to refactoring providers when registering actions.</summary>
public readonly struct CodeRefactoringContext
{
    private readonly Action<CodeAction> _registerRefactoring;

    internal CodeRefactoringContext(
        Document document,
        TextSpan span,
        Action<CodeAction> registerRefactoring,
        CancellationToken cancellationToken)
    {
        Document = document ?? throw new ArgumentNullException(nameof(document));
        Span = span;
        _registerRefactoring = registerRefactoring ?? throw new ArgumentNullException(nameof(registerRefactoring));
        CancellationToken = cancellationToken;
    }

    public Document Document { get; }

    public TextSpan Span { get; }

    public CancellationToken CancellationToken { get; }

    public void RegisterRefactoring(CodeAction action)
    {
        if (action is null)
            throw new ArgumentNullException(nameof(action));

        _registerRefactoring(action);
    }
}
