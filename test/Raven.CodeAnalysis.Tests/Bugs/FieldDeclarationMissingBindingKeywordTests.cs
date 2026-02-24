using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class FieldDeclarationMissingBindingKeywordTests : DiagnosticTestBase
{
    [Fact]
    public void MemberWithoutBindingKeyword_ParsesAsProperty()
    {
        const string code = """
        class Foo {
            name: string = ""
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV0914")
                .WithSpan(2, 5, 2, 9)
                .WithArguments("name")
        ]);
        verifier.Verify();
    }

    [Fact]
    public void MemberWithoutBindingKeyword_ParsesSinglePropertyMember()
    {
        const string code = """
        class Foo {
            name: string = ""
        }
        """;

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        var type = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));
        var property = Assert.IsType<PropertyDeclarationSyntax>(Assert.Single(type.Members));

        Assert.Equal("name", property.Identifier.Text);
    }
}
