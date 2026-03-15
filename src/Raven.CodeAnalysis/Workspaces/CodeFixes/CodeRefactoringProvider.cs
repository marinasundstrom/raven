namespace Raven.CodeAnalysis;

/// <summary>Base type for providers that register context-driven code actions without diagnostics.</summary>
public abstract class CodeRefactoringProvider
{
    public abstract void RegisterRefactorings(CodeRefactoringContext context);
}
