using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Tests.Semantics.Lowering;

public sealed class UnreachableCodeOptimizerTests
{
    [Fact]
    public void Rewrite_RemovesStatementAfterReturn()
    {
        var compilation = Compilation.Create(
            "unreachable_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var returnStatement = new BoundReturnStatement(expression: null);
        var unreachable = new BoundExpressionStatement(new BoundLiteralExpression(
            BoundLiteralExpressionKind.TrueLiteral,
            true,
            booleanType));
        var block = new BoundBlockStatement([returnStatement, unreachable]);

        var rewritten = Assert.IsType<BoundBlockStatement>(UnreachableCodeOptimizer.Rewrite(block));

        Assert.Equal([returnStatement], rewritten.Statements);
    }
}
