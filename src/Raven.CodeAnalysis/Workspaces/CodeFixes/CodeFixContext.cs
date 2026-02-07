namespace Raven.CodeAnalysis;

/// <summary>Context supplied to code-fix providers when registering fixes.</summary>
public readonly struct CodeFixContext
{
    private readonly Action<CodeAction> _registerCodeFix;

    internal CodeFixContext(
        Document document,
        Diagnostic diagnostic,
        Action<CodeAction> registerCodeFix,
        CancellationToken cancellationToken)
    {
        Document = document ?? throw new ArgumentNullException(nameof(document));
        Diagnostic = diagnostic ?? throw new ArgumentNullException(nameof(diagnostic));
        _registerCodeFix = registerCodeFix ?? throw new ArgumentNullException(nameof(registerCodeFix));
        CancellationToken = cancellationToken;
    }

    public Document Document { get; }

    public Diagnostic Diagnostic { get; }

    public CancellationToken CancellationToken { get; }

    public void RegisterCodeFix(CodeAction action)
    {
        if (action is null)
            throw new ArgumentNullException(nameof(action));

        _registerCodeFix(action);
    }
}
