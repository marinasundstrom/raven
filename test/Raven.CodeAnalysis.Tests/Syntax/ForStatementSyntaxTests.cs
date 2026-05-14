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

    [Fact]
    public void For_WithOuterValImplicitTypedPositionalTarget_ParsesTargetAnnotations()
    {
        const string source = """
for val (key: string, value: int) in items {
}
""";

        var tree = SyntaxTree.ParseText(source);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var forStatement = Assert.IsType<ForStatementSyntax>(statement);
        var target = Assert.IsType<PositionalPatternSyntax>(forStatement.Target);

        Assert.Collection(
            target.Elements,
            element =>
            {
                element.NameColon.ShouldBeNull();
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var typed = Assert.IsType<TypedVariableDesignationSyntax>(variable.Designation);
                var single = Assert.IsType<SingleVariableDesignationSyntax>(typed.Designation);
                single.Identifier.ValueText.ShouldBe("key");
                typed.TypeAnnotation.Type.ToString().ShouldBe("string");
            },
            element =>
            {
                element.NameColon.ShouldBeNull();
                var variable = Assert.IsType<VariablePatternSyntax>(element.Pattern);
                var typed = Assert.IsType<TypedVariableDesignationSyntax>(variable.Designation);
                var single = Assert.IsType<SingleVariableDesignationSyntax>(typed.Designation);
                single.Identifier.ValueText.ShouldBe("value");
                typed.TypeAnnotation.Type.ToString().ShouldBe("int");
            });
    }

    [Fact]
    public void For_WithOuterValImplicitTypedSequenceTarget_ParsesTargetAnnotations()
    {
        const string source = """
for val [head: string, ..tail: string[]] in items {
}
""";

        var tree = SyntaxTree.ParseText(source);
        var statement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single()).Statement;
        var forStatement = Assert.IsType<ForStatementSyntax>(statement);
        var target = Assert.IsType<SequencePatternSyntax>(forStatement.Target);

        var head = Assert.IsType<VariablePatternSyntax>(target.Elements[0].Pattern);
        var headType = Assert.IsType<TypedVariableDesignationSyntax>(head.Designation);
        headType.TypeAnnotation.Type.ToString().ShouldBe("string");

        var tail = Assert.IsType<VariablePatternSyntax>(target.Elements[1].Pattern);
        var tailType = Assert.IsType<TypedVariableDesignationSyntax>(tail.Designation);
        tailType.TypeAnnotation.Type.ToString().ShouldBe("string[]");
    }
}
