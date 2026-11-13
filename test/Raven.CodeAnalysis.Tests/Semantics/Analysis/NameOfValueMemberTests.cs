using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Semantics.Tests;

namespace Raven.CodeAnalysis.Tests.Semantics.Analysis;

public sealed class NameOfValueMemberTests : CompilationTestBase
{
    [Theory]
    [InlineData("import System.*\nlet name = nameof(AttributeUsageAttribute.AllowMultiple)", "AllowMultiple")]
    [InlineData("let name = nameof(System.AttributeUsageAttribute.AllowMultiple)", "AllowMultiple")]
    [InlineData("class Box { val Text: string = \"\" }\nlet name = nameof(Box.Text)", "Text")]
    [InlineData("let name = nameof(string.Length)", "Length")]
    [InlineData("open class Base { val Text: string = \"\" }\nclass Box : Base {}\nlet name = nameof(Box.Text)", "Text")]
    [InlineData("class Box<T> { val Text: string = \"\" }\nlet name = nameof(Box<int>.Text)", "Text")]
    [InlineData("class Box { func Read() -> int => 1 }\nlet name = nameof(Box.Read)", "Read")]
    public void TypeQualifiedInstanceMembersCanBeNamed(string source, string expected)
    {
        var (compilation, tree) = CreateCompilation(source);
        var expression = tree.GetRoot().DescendantNodes().OfType<NameOfExpressionSyntax>().Single();
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.Equal(expected, Assert.IsType<BoundNameOfExpression>(compilation.GetSemanticModel(tree).GetBoundNode(expression)).Name);
    }

    [Theory]
    [InlineData("class Box { private val Text: string = \"\" }\nlet name = nameof(Box.Text)")]
    [InlineData("class Box { val Text: string = \"\" }\nlet text = Box.Text")]
    public void TypeQualifiedNamingDoesNotBypassAccessRulesOrAllowRuntimeReads(string source)
    {
        var (compilation, _) = CreateCompilation(source);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

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
