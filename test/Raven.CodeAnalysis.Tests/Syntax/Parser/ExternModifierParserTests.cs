using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ExternModifierParserTests
{
    [Fact]
    public void ExternFunctionStatement_ParsesAsFunction()
    {
        var tree = SyntaxTree.ParseText("extern func NativeSleep(milliseconds: int) -> unit;");
        var root = tree.GetRoot();

        Assert.Empty(tree.GetDiagnostics());

        var global = Assert.IsType<GlobalStatementSyntax>(Assert.Single(root.Members));
        var function = Assert.IsType<FunctionStatementSyntax>(global.Statement);
        Assert.Contains(function.Modifiers, modifier => modifier.Kind == SyntaxKind.ExternKeyword);
        Assert.Null(function.Body);
        Assert.Null(function.ExpressionBody);
    }

    [Fact]
    public void ExternMethodDeclaration_ParsesAsMethod()
    {
        var tree = SyntaxTree.ParseText(
            """
            class Native {
                public extern static Sleep(milliseconds: int) -> unit;
            }
            """
        );
        var root = tree.GetRoot();

        Assert.Empty(tree.GetDiagnostics());

        var type = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));
        var method = Assert.IsType<MethodDeclarationSyntax>(Assert.Single(type.Members));
        Assert.Contains(method.Modifiers, modifier => modifier.Kind == SyntaxKind.ExternKeyword);
        Assert.Contains(method.Modifiers, modifier => modifier.Kind == SyntaxKind.StaticKeyword);
        Assert.Null(method.Body);
        Assert.Null(method.ExpressionBody);
    }
}
