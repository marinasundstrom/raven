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

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void IfStatement_WithMissingCondition()
    {
        string testCode =
            """
            if() {
            
            }
            """;

        var verifier = CreateVerifier(testCode,
               [
                    new DiagnosticResult("RAV1525").WithLocation(1, 4).WithArguments(')'),
               ]);

        verifier.Verify();
    }

    [Fact]
    public void IfStatement_MissingBlock()
    {
        string testCode =
            """
            if(x)
            
            }
            """;

        var verifier = CreateVerifier(testCode,
                 [
                      new DiagnosticResult("RAV1525").WithLocation(3, 1).WithArguments('}'),
               ]);

        verifier.Verify();
    }

    [Fact]
    public void IfStatement_MissingBlock_WithReturnStatement()
    {
        string testCode =
            """
            if(x)
                return 2;
            }
            """;

        var verifier = CreateVerifier(testCode,
                 [
                      new DiagnosticResult("RAV1525").WithLocation(3, 1).WithArguments('}'),
               ]);

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

        var verifier = CreateVerifier(testCode);

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

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}