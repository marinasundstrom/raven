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
}
