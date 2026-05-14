using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class WhileStatementSyntaxTests
{
    [Fact]
    public void WhilePatternStatement_ParsesAsDedicatedNode()
    {
        const string testCode = """
while val (.Ok, value) = Func() {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var whileBinding = Assert.IsType<WhilePatternStatementSyntax>(statement);
        var pattern = Assert.IsType<PositionalPatternSyntax>(whileBinding.Pattern);

        whileBinding.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        whileBinding.Expression.ShouldBeOfType<InvocationExpressionSyntax>();
        pattern.Elements.Count.ShouldBe(2);
        pattern.Elements[0].Pattern.ShouldBeOfType<MemberPatternSyntax>();
        pattern.Elements[1].Pattern.ShouldBeOfType<VariablePatternSyntax>();
    }

    [Fact]
    public void WhilePatternStatement_WithImplicitTypedPositionalElements_ParsesVariablePatterns()
    {
        const string testCode = """
while val (key: string, value: int) = Next() {
}
""";

        var tree = SyntaxTree.ParseText(testCode);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var whileBinding = Assert.IsType<WhilePatternStatementSyntax>(statement);
        var pattern = Assert.IsType<PositionalPatternSyntax>(whileBinding.Pattern);

        Assert.Collection(
            pattern.Elements,
            element =>
            {
                element.NameColon.ShouldBeNull();
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var typed = Assert.IsType<TypedVariableDesignationSyntax>(variable.Designation);
                typed.TypeAnnotation.Type.ToString().ShouldBe("string");
            },
            element =>
            {
                element.NameColon.ShouldBeNull();
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var typed = Assert.IsType<TypedVariableDesignationSyntax>(variable.Designation);
                typed.TypeAnnotation.Type.ToString().ShouldBe("int");
            });
    }
}
