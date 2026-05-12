using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class ForStatementSyntaxTests
{
    [Fact]
    public void For_WithTypedIdentifierTarget_ParsesTargetAnnotation()
    {
        const string source = """
for x: int in items {
}
""";

        var tree = SyntaxTree.ParseText(source);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var forStatement = Assert.IsType<ForStatementSyntax>(statement);
        var target = Assert.IsType<VariablePatternSyntax>(forStatement.Target);
        var typed = Assert.IsType<TypedVariableDesignationSyntax>(target.Designation);
        var single = Assert.IsType<SingleVariableDesignationSyntax>(typed.Designation);

        forStatement.BindingKeyword.Kind.ShouldBe(SyntaxKind.None);
        target.BindingKeyword.Kind.ShouldBe(SyntaxKind.None);
        single.Identifier.ValueText.ShouldBe("x");
        typed.TypeAnnotation.Type.ToString().ShouldBe("int");
    }

    [Fact]
    public void For_WithOuterValTypedIdentifierTarget_ParsesTargetAnnotation()
    {
        const string source = """
for val x: int in items {
}
""";

        var tree = SyntaxTree.ParseText(source);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var forStatement = Assert.IsType<ForStatementSyntax>(statement);
        var target = Assert.IsType<VariablePatternSyntax>(forStatement.Target);
        var typed = Assert.IsType<TypedVariableDesignationSyntax>(target.Designation);
        var single = Assert.IsType<SingleVariableDesignationSyntax>(typed.Designation);

        forStatement.BindingKeyword.Kind.ShouldBe(SyntaxKind.ValKeyword);
        target.BindingKeyword.Kind.ShouldBe(SyntaxKind.None);
        single.Identifier.ValueText.ShouldBe("x");
        typed.TypeAnnotation.Type.ToString().ShouldBe("int");
    }
}
