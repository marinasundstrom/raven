using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class FieldDeclarationMissingLetOrVarTests : DiagnosticTestBase
{
    [Fact]
    public void FieldWithoutLetOrVar_ReportsDiagnostic()
    {
        const string code = """
        class Foo {
            name: string = ""
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV1007").WithAnySpan(),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void FieldWithoutLetOrVar_ParsesSingleMember()
    {
        const string code = """
        class Foo {
            name: string = ""
        }
        """;

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        var type = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));
        var field = Assert.IsType<FieldDeclarationSyntax>(Assert.Single(type.Members));

        var declarator = Assert.Single(field.Declaration.Declarators);
        Assert.Equal("name", declarator.Identifier.Text);
    }
}
