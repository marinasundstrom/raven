namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class PatternSyntax : ExpressionOrPatternSyntax
{
    public PatternSyntax(
        SyntaxKind kind,
        GreenNode[] slots,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }
}

internal partial class UnaryPatternSyntax : PatternSyntax
{
    public UnaryPatternSyntax(
        SyntaxToken operatorToken,
        PatternSyntax pattern,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.UnaryPatternSyntax,
              [
                      operatorToken ?? throw new ArgumentNullException(nameof(operatorToken)),
                      pattern ?? throw new ArgumentNullException(nameof(pattern))
              ],
              diagnostics)
    {
    }
}


internal partial class DeclarationPatternSyntax : PatternSyntax
{
    public DeclarationPatternSyntax(
        TypeSyntax type,
        VariableDesignationSyntax variableDesignation,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.DeclarationPatternSyntax,
              [
                      type ?? throw new ArgumentNullException(nameof(type)),
                      variableDesignation ?? throw new ArgumentNullException(nameof(variableDesignation))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static UnaryPatternSyntax UnaryPattern(
        SyntaxToken operatorToken,
        PatternSyntax pattern,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(operatorToken, pattern, diagnostics);

    public static DeclarationPatternSyntax DeclarationPattern(
        TypeSyntax type,
        VariableDesignationSyntax variableDesignation,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(type, variableDesignation, diagnostics);
}