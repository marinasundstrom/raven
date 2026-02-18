using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class IfStatementSyntaxTest : DiagnosticTestBase
{
    [Fact]
    public void IfStatement()
    {
        string testCode =
            """
            if(x) {
            
            }
            """;

        var verifier = CreateVerifier(testCode, disabledDiagnostics: [CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id]);

        verifier.Verify();
    }

    [Fact]
    public void IfStatement_WithElseClause()
    {
        string testCode =
            """
            if(x) {
            
            } else {
            
            }
            """;

        var verifier = CreateVerifier(testCode, disabledDiagnostics: [CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id]);

        verifier.Verify();
    }

    [Fact]
    public void IfStatement_WithElseClause_WithIfStatement()
    {
        string testCode =
            """
            if(x) {
            
            } else if (y) {
            
            }
            """;

        var verifier = CreateVerifier(testCode, disabledDiagnostics: [CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id]);

        verifier.Verify();
    }
}
