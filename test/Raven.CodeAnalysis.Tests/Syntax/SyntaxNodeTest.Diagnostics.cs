namespace Raven.CodeAnalysis.Syntax.Tests;

using Raven.CodeAnalysis.Syntax;
using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public partial class SyntaxNodeTest
{
    [Fact]
    public void ReturnStatement_WithMissingSemicolon_ReportsDiagnostic()
    {
        var stmt = ReturnStatement(ReturnKeyword, MissingToken(SyntaxKind.SemicolonToken));
        var diagnostics = stmt.GetDiagnostics().ToArray();
        diagnostics.ShouldContain(d => d.Descriptor.Id == "RAV1002");
    }

    [Fact]
    public void Block_WithMissingCloseBrace_ReportsDiagnostic()
    {
        var block = Block(OpenBraceToken, List<StatementSyntax>(), MissingToken(SyntaxKind.CloseBraceToken));
        var diagnostics = block.GetDiagnostics().ToArray();
        diagnostics.ShouldContain(d => d.Descriptor.Id == "RAV1003");
    }
}
