using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ObjectInitializerParserTests
{
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
        var assignment = Assert.Single(initializer.Assignments);

        Assert.Equal("Clicked", assignment.Name.Identifier.ValueText);
        Assert.Equal(SyntaxKind.PlusEqualsToken, assignment.EqualsToken.Kind);
    }
}
