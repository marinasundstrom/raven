﻿namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ElseClauseSyntax : SyntaxNode
{
    public ElseClauseSyntax(
        SyntaxToken elseKeyword,
        StatementSyntax statement,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ElseClause,
              [
                      elseKeyword ?? throw new ArgumentNullException(nameof(elseKeyword)),
                      statement ?? throw new ArgumentNullException(nameof(statement))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ElseClauseSyntax ElseClause(
        SyntaxToken elseKeyword,
        StatementSyntax statement,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(elseKeyword, statement, diagnostics);
}