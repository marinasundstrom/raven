using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class TrailingBlockParserTests
{
    [Fact]
    public void TrailingBlock_AssignmentStatement_WithPlusEquals_ParsesInBody()
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
        var trailingBlock = Assert.IsType<TrailingBlockExpressionSyntax>(invocation.TrailingBlock);
        var assignment = Assert.IsType<AssignmentStatementSyntax>(Assert.Single(trailingBlock.Body.Statements));

        Assert.Equal(SyntaxKind.AddAssignmentStatement, assignment.Kind);
        var left = Assert.IsType<IdentifierNameSyntax>(assignment.Left);
        Assert.Equal("Clicked", left.Identifier.ValueText);
        Assert.Equal(SyntaxKind.PlusEqualsToken, assignment.OperatorToken.Kind);
    }

    [Fact]
    public void TrailingBlock_OmittedArgumentList_ParsesAsInvocationWithBody()
    {
        var tree = SyntaxTree.ParseText(
            """
            val window = Window {
                StackPanel()
            }
            """);

        var root = (CompilationUnitSyntax)tree.GetRoot();
        var declaration = root.DescendantNodes().OfType<LocalDeclarationStatementSyntax>().Single();
        var invocation = Assert.IsType<InvocationExpressionSyntax>(declaration.Declaration.Declarators.Single().Initializer!.Value);

        Assert.True(invocation.ArgumentList.OpenParenToken.IsMissing);
        Assert.NotNull(invocation.TrailingBlock);
        Assert.Single(invocation.TrailingBlock!.Body.Statements);
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

    [Fact]
    public void WithInitializer_ExpressionEntry_ParsesAsEntry()
    {
        var tree = SyntaxTree.ParseText(
            """
            val window = Window with {
                StackPanel with {
                }
            }
            """);

        var root = (CompilationUnitSyntax)tree.GetRoot();
        var declaration = root.DescendantNodes().OfType<LocalDeclarationStatementSyntax>().Single();
        var initializer = Assert.IsType<WithExpressionSyntax>(declaration.Declaration.Declarators.Single().Initializer!.Value);
        var entry = Assert.IsType<WithExpressionEntrySyntax>(Assert.Single(initializer.Entries));

        Assert.IsType<WithExpressionSyntax>(entry.Expression);
    }
}
