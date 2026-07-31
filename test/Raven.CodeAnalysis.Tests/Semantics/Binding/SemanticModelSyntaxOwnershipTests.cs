using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SemanticModelSyntaxOwnershipTests : CompilationTestBase
{
    [Fact]
    public void AuthoritativeSemanticQueries_RejectNodesFromAnotherSyntaxTree()
    {
        var source = "let value: int = 1";
        var modelTree = SyntaxTree.ParseText(source);
        var foreignTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(modelTree);
        var model = compilation.GetSemanticModel(modelTree);
        var foreignRoot = foreignTree.GetRoot();
        var declarator = foreignRoot.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var expression = declarator.Initializer!.Value;

        Assert.Throws<ArgumentException>(() => model.GetDeclaredSymbol(declarator));
        Assert.Throws<ArgumentException>(() => model.GetSymbolInfo(expression));
        Assert.Throws<ArgumentException>(() => model.GetTypeInfo(expression));
        Assert.Throws<ArgumentException>(() => model.GetOperation(expression));
    }

    [Fact]
    public void AuthoritativeSemanticQueries_RejectDetachedNodes()
    {
        var tree = SyntaxTree.ParseText("let value = 1");
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var detached = SyntaxFactory.IdentifierName(SyntaxFactory.Identifier("value"));

        Assert.Throws<ArgumentException>(() => model.GetSymbolInfo(detached));
        Assert.Throws<ArgumentException>(() => model.GetTypeInfo(detached));
        Assert.Throws<ArgumentException>(() => model.GetOperation(detached));
    }

    [Fact]
    public void SpecializedSemanticQueries_RejectNodesFromAnotherSyntaxTree()
    {
        const string source = """
func Test() {
    let offset = 1
    let add = (value: int) => value + offset
}
""";
        var modelTree = SyntaxTree.ParseText(source);
        var foreignTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(modelTree);
        var model = compilation.GetSemanticModel(modelTree);
        var functionExpression = foreignTree.GetRoot().DescendantNodes().OfType<FunctionExpressionSyntax>().Single();
        var parameter = functionExpression.DescendantNodes().OfType<ParameterSyntax>().Single();

        Assert.Throws<ArgumentException>(() => model.GetCapturedVariables(functionExpression));
        Assert.Throws<ArgumentException>(() => model.GetFunctionExpressionParameterSymbol(parameter));
    }

    [Fact]
    public void ExpandedDeclarationQuery_RejectsAttributeFromAnotherSyntaxTree()
    {
        const string source = "[Obsolete] class C {}";
        var modelTree = SyntaxTree.ParseText(source);
        var foreignTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(modelTree);
        var model = compilation.GetSemanticModel(modelTree);
        var attribute = foreignTree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

        Assert.Throws<ArgumentException>(() => model.GetExpandedDeclaration(attribute));
    }
}
