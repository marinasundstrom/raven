using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class PointerTypeSyntaxTests
{
    [Fact]
    public void PointerType_InVariableDeclaration_Parses()
    {
        var code = "let ptr: *int = 0";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;
        Assert.IsType<PointerTypeSyntax>(typeSyntax);
    }

    [Fact]
    public void PointerDereference_ParsesAsUnaryDereferenceExpression()
    {
        var code = "let value = *ptr";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var initializer = local.Declaration.Declarators[0].Initializer!;

        var dereference = Assert.IsType<UnaryExpressionSyntax>(initializer.Value);
        Assert.Equal(SyntaxKind.DereferenceExpression, dereference.Kind);
        Assert.Equal(SyntaxKind.StarToken, dereference.OperatorToken.Kind);
    }

    [Fact]
    public void PointerArrowMemberAccess_ParsesOperatorToken()
    {
        var code = "let value = ptr->Field";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var initializer = local.Declaration.Declarators[0].Initializer!;

        var memberAccess = Assert.IsType<MemberAccessExpressionSyntax>(initializer.Value);
        Assert.Equal(SyntaxKind.PointerMemberAccessExpression, memberAccess.Kind);
        Assert.Equal(SyntaxKind.ArrowToken, memberAccess.OperatorToken.Kind);
    }

    [Fact]
    public void PointerDereferenceAssignment_ParsesAsSeparateAssignmentStatement()
    {
        var code = """
class Test {
    static Run() {
        var value = 41
        let pointer: *int = &value
        *pointer = 42
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var body = tree.GetRoot().DescendantNodes().OfType<BlockStatementSyntax>().Single();
        Assert.Equal(3, body.Statements.Count);

        var assignment = Assert.IsType<AssignmentStatementSyntax>(body.Statements[2]);
        var left = Assert.IsType<UnaryExpressionSyntax>(assignment.Left);
        Assert.Equal(SyntaxKind.DereferenceExpression, left.Kind);
    }

    [Fact]
    public void UnsafeBlock_ParsesAsUnsafeStatement()
    {
        var code = """
func test() -> int {
    var value = 0
    unsafe {
        let pointer: *int = &value
        *pointer = 1
    }
    return value
}
""";

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var unsafeStatement = root.DescendantNodes().OfType<UnsafeStatementSyntax>().Single();
        Assert.Equal(SyntaxKind.UnsafeKeyword, unsafeStatement.UnsafeKeyword.Kind);
        Assert.NotNull(unsafeStatement.Block);
    }

    [Fact]
    public void UnsafeFunctionModifier_Parses()
    {
        var code = "unsafe func test() -> int { return 0 }";
        var tree = SyntaxTree.ParseText(code);
        var function = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        Assert.Contains(function.Modifiers, m => m.Kind == SyntaxKind.UnsafeKeyword);
    }
}
