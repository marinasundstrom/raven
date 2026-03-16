using System.Linq;

using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Syntax;

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

    [Fact]
    public void IfStatement_WithoutBraces_ParsesWithoutBraceDiagnostic()
    {
        const string testCode = """
if x
    return 0
else
    return 1
""";

        var verifier = CreateVerifier(
            testCode,
            disabledDiagnostics: [CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id]);

        verifier.Verify();
    }

    [Fact]
    public void IfPatternStatement_ParsesAsDedicatedNode()
    {
        const string testCode = """
if val (id, name) = person {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var ifBinding = Assert.IsType<IfPatternStatementSyntax>(statement);

        ifBinding.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        ifBinding.Pattern.ShouldBeOfType<PositionalPatternSyntax>();
        ifBinding.Expression.ShouldBeOfType<IdentifierNameSyntax>();
    }

    [Fact]
    public void IfPatternStatement_WithRecursivePattern_ParsesNestedImplicitBindings()
    {
        const string testCode = """
if val Person(1, name, _) = person {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var ifBinding = Assert.IsType<IfPatternStatementSyntax>(statement);
        var pattern = Assert.IsType<NominalDeconstructionPatternSyntax>(ifBinding.Pattern);
        var arguments = pattern.ArgumentList!.Arguments;

        arguments.Count.ShouldBe(3);
        arguments[1].ShouldBeOfType<VariablePatternSyntax>();
    }
}
