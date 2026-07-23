using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ObjectInitializerParserTests
{
    [Fact]
    public void ObjectInitializer_OmittedArgumentList_ParsesOnTypeName()
    {
        var tree = SyntaxTree.ParseText(
            """
            val foo = Foo {
                Name = "Foo"
            }
            """);

        var root = (CompilationUnitSyntax)tree.GetRoot();
        var declaration = root.DescendantNodes().OfType<LocalDeclarationStatementSyntax>().Single();
        var invocation = Assert.IsType<InvocationExpressionSyntax>(declaration.Declaration.Declarators.Single().Initializer!.Value);
        var initializer = Assert.IsType<ObjectInitializerExpressionSyntax>(invocation.Initializer);
        var assignment = Assert.IsType<ObjectInitializerAssignmentEntrySyntax>(Assert.Single(initializer.Entries));

        Assert.True(invocation.ArgumentList.OpenParenToken.IsMissing);
        Assert.Equal("Name", assignment.Name.Identifier.ValueText);
        Assert.Equal(SyntaxKind.EqualsToken, assignment.EqualsToken.Kind);
    }

    [Fact]
    public void ObjectInitializer_ParsesAfterConstructorArguments()
    {
        var tree = SyntaxTree.ParseText(
            """
            val bar = Bar("Foo") {
                Age = 42
            }
            """);

        var root = (CompilationUnitSyntax)tree.GetRoot();
        var declaration = root.DescendantNodes().OfType<LocalDeclarationStatementSyntax>().Single();
        var invocation = Assert.IsType<InvocationExpressionSyntax>(declaration.Declaration.Declarators.Single().Initializer!.Value);
        var initializer = Assert.IsType<ObjectInitializerExpressionSyntax>(invocation.Initializer);
        var assignment = Assert.IsType<ObjectInitializerAssignmentEntrySyntax>(Assert.Single(initializer.Entries));

        Assert.Single(invocation.ArgumentList.Arguments);
        Assert.Equal("Age", assignment.Name.Identifier.ValueText);
        Assert.Equal(SyntaxKind.EqualsToken, assignment.EqualsToken.Kind);
    }

    [Fact]
    public void ObjectInitializer_AssignmentEntry_WithPlusEquals_ParsesOperatorToken()
    {
        var tree = SyntaxTree.ParseText(
            """
            val button = Button() {
                Clicked += handler
            }
            """);

        var root = (CompilationUnitSyntax)tree.GetRoot();
        var declaration = root.DescendantNodes().OfType<LocalDeclarationStatementSyntax>().Single();
        var invocation = Assert.IsType<InvocationExpressionSyntax>(declaration.Declaration.Declarators.Single().Initializer!.Value);
        var initializer = Assert.IsType<ObjectInitializerExpressionSyntax>(invocation.Initializer);
        var assignment = Assert.IsType<ObjectInitializerAssignmentEntrySyntax>(Assert.Single(initializer.Entries));

        Assert.Equal("Clicked", assignment.Name.Identifier.ValueText);
        Assert.Equal(SyntaxKind.PlusEqualsToken, assignment.EqualsToken.Kind);
    }

    [Fact]
    public void WithInitializer_AssignmentEntry_WithPlusEquals_ParsesOperatorToken()
    {
        var tree = SyntaxTree.ParseText(
            """
            val button = Button with {
                Clicked += handler
            }
            """);

        var root = (CompilationUnitSyntax)tree.GetRoot();
        var declaration = root.DescendantNodes().OfType<LocalDeclarationStatementSyntax>().Single();
        var initializer = Assert.IsType<WithExpressionSyntax>(declaration.Declaration.Declarators.Single().Initializer!.Value);
        var assignment = Assert.IsType<WithAssignmentSyntax>(Assert.Single(initializer.Entries));

        Assert.Equal("Clicked", assignment.Name.Identifier.ValueText);
        Assert.Equal(SyntaxKind.PlusEqualsToken, assignment.EqualsToken.Kind);
    }
}
