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
        SyntaxKind kind,
        SyntaxToken operatorToken,
        PatternSyntax pattern,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              kind,
              [
                      operatorToken ?? throw new ArgumentNullException(nameof(operatorToken)),
                      pattern ?? throw new ArgumentNullException(nameof(pattern))
              ],
              diagnostics)
    {
    }
}

internal partial class BinaryPatternSyntax : PatternSyntax
{
    public BinaryPatternSyntax(
        SyntaxKind kind,
        PatternSyntax left,
        SyntaxToken operatorToken,
        PatternSyntax right,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              kind,
              [
                      left ?? throw new ArgumentNullException(nameof(left)),
                      operatorToken ?? throw new ArgumentNullException(nameof(operatorToken)),
                      right ?? throw new ArgumentNullException(nameof(right))
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
              SyntaxKind.DeclarationPattern,
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
        SyntaxKind kind,
        SyntaxToken operatorToken,
        PatternSyntax pattern,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(kind, operatorToken, pattern, diagnostics);

    public static BinaryPatternSyntax BinaryPattern(
        SyntaxKind kind,
        PatternSyntax left,
        SyntaxToken operatorToken,
        PatternSyntax right,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(kind, left, operatorToken, right, diagnostics);

    public static DeclarationPatternSyntax DeclarationPattern(
        TypeSyntax type,
        VariableDesignationSyntax variableDesignation,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(type, variableDesignation, diagnostics);
}