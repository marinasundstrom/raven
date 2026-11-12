using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Semantics.Tests;

namespace Raven.CodeAnalysis.Tests.Semantics.Analysis;

public sealed class NameOfValueMemberTests : CompilationTestBase
{
    [Theory]
    [InlineData("func Name(value: string) -> string { return nameof(value.Length) }")]
    [InlineData("class Box { val Text: string = \"\" }\nfunc Name(value: Box) -> string { return nameof(value.Text.Length) }")]
    [InlineData("let value = \"hello\"\nlet name = nameof(value.Length)")]
    [InlineData("open class Parent { init(name: string) {} }\nclass Child : Parent { init(value: string) : base(nameof(value.Length)) {} }")]
    public void ValueReceiverMemberNamesResolveInLexicalScope(string source)
    {
        var (compilation, tree) = CreateCompilation(source);
        var nameOf = tree.GetRoot().DescendantNodes().OfType<NameOfExpressionSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.Equal("Length", Assert.IsType<BoundNameOfExpression>(model.GetBoundNode(nameOf)).Name);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.Equal("Length", Assert.IsType<BoundNameOfExpression>(model.GetBoundNode(nameOf)).Name);
    }
}
