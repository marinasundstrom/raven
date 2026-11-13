using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Macros.Tests;

public sealed class FreestandingMacroContextTests
{
    [Fact]
    public void ArgumentContext_NormalizesTypeMemberInvocation()
    {
        var (compilation, semanticModel, invocation) = CreateMemberInvocation("Generate!(1)");

        var context = new FreestandingMacroContext(compilation, semanticModel, invocation);

        Assert.Same(compilation, context.Compilation);
        Assert.Same(semanticModel, context.SemanticModel);
        Assert.Same(invocation, context.Syntax);
        Assert.Equal("Generate", context.Name.ToString());
        Assert.Equal(SyntaxKind.ExclamationToken, context.ExclamationToken.Kind);
        Assert.Single(context.ArgumentList.Arguments);
        Assert.Single(context.Arguments);
        Assert.Null(context.TokenTree);
        var argument = Assert.Single(context.Arguments);
        var expectedType = semanticModel.GetMacroArgumentType(argument.Expression);
        Assert.True(SymbolEqualityComparer.Default.Equals(expectedType, argument.SemanticType));
    }

    [Fact]
    public void TokenTreeContext_NormalizesTypeMemberInvocation()
    {
        var (compilation, semanticModel, invocation) = CreateMemberInvocation(
            "Generate! { value + 1 }");

        var context = new TokenTreeMacroContext(compilation, semanticModel, invocation);
        var parsed = context.ParseExpressionResult();

        Assert.Same(compilation, context.Compilation);
        Assert.Same(semanticModel, context.SemanticModel);
        Assert.Same(invocation, context.Syntax);
        Assert.Equal("Generate", context.Name.ToString());
        Assert.Same(invocation.TokenTree, context.TokenTree);
        Assert.False(parsed.HasErrors, string.Join(System.Environment.NewLine, parsed.Diagnostics));
        Assert.Equal("value + 1", parsed.Syntax.ToString());
    }

    [Fact]
    public void ExpressionContext_UsesRealSemanticArgumentServices()
    {
        var tree = SyntaxTree.ParseText("let value = Generate!(1)");
        var compilation = Compilation.Create("ExpressionContext", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default).AddSyntaxTrees(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        var context = new FreestandingMacroContext(compilation, model, invocation);
        var argument = Assert.Single(context.Arguments);
        Assert.True(SymbolEqualityComparer.Default.Equals(model.GetMacroArgumentType(argument.Expression), argument.SemanticType));
        Assert.NotEqual(TypeKind.Error, argument.SemanticType!.TypeKind);
    }

    private static (
        Compilation Compilation,
        SemanticModel SemanticModel,
        FreestandingMacroMemberDeclarationSyntax Invocation) CreateMemberInvocation(string source)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Model {
                {{source}}
            }
            """);
        var compilation = Compilation.Create(
                "MemberMacroContext",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroMemberDeclarationSyntax>()
            .Single();

        return (compilation, compilation.GetSemanticModel(tree), invocation);
    }
}
