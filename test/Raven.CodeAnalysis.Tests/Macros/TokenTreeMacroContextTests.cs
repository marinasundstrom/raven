using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Macros.Tests;

public sealed class TokenTreeMacroContextTests
{
    [Fact]
    public void ParseExpressionResult_RetainsAuthoredPosition()
    {
        var context = CreateContext("value + 1");
        var start = context.GetBodyText().IndexOf("value", StringComparison.Ordinal);

        var result = context.ParseExpressionResult();

        Assert.False(result.HasErrors);
        Assert.IsType<InfixOperatorExpressionSyntax>(result.Syntax);
        Assert.Equal(context.BodySpan.Start + start, result.Syntax.Span.Start);
    }

    [Fact]
    public void ParseTypeResult_ParsesSelectedSourceBackedSpan()
    {
        var context = CreateContext("type {{ string? }}");
        var body = context.GetBodyText();
        var start = body.IndexOf("string?", StringComparison.Ordinal);
        var span = new TextSpan(start, "string?".Length);

        var result = context.ParseTypeResult(span);

        Assert.False(result.HasErrors);
        Assert.IsType<NullableTypeSyntax>(result.Syntax);
        Assert.Equal(context.BodySpan.Start + start, result.Syntax.Span.Start);
    }

    [Fact]
    public void ParsePatternResult_ParsesPatternAtAuthoredLocation()
    {
        var context = CreateContext("let value");
        var start = context.GetBodyText().IndexOf("let", StringComparison.Ordinal);

        var result = context.ParsePatternResult();

        Assert.False(result.HasErrors);
        Assert.IsType<VariablePatternSyntax>(result.Syntax);
        Assert.Equal(context.BodySpan.Start + start, result.Syntax.Span.Start);
    }

    [Fact]
    public void ParseCompilationUnitResult_ParsesDeclarations()
    {
        var context = CreateContext("class Widget { }");

        var result = context.ParseCompilationUnitResult();

        Assert.False(result.HasErrors);
        Assert.IsType<ClassDeclarationSyntax>(Assert.Single(result.Syntax.Members));
    }

    [Fact]
    public void ParseTypeResult_MapsDiagnosticsIntoAuthoredBody()
    {
        var context = CreateContext("List<");

        var result = context.ParseTypeResult();

        Assert.True(result.HasErrors);
        Assert.All(
            result.Diagnostics,
            diagnostic => Assert.True(
                diagnostic.Location.SourceSpan.Start >= context.BodySpan.Start &&
                diagnostic.Location.SourceSpan.End <= context.BodySpan.End));
    }

    [Fact]
    public void ParseMemberDeclarationResult_ParsesOneDeclaration()
    {
        var context = CreateContext("class Widget { }");

        var result = context.ParseMemberDeclarationResult();

        Assert.False(result.HasErrors);
        Assert.IsType<ClassDeclarationSyntax>(result.Syntax);
    }

    [Fact]
    public void ParseMemberDeclarationResult_RejectsMultipleDeclarations()
    {
        var context = CreateContext("class First { } class Second { }");

        var result = context.ParseMemberDeclarationResult();

        Assert.True(result.HasErrors);
        Assert.Equal("First", Assert.IsType<ClassDeclarationSyntax>(result.Syntax).Identifier.ValueText);
        var diagnostic = Assert.Single(result.Diagnostics, static diagnostic => diagnostic.Descriptor.Id == "RAVM022");
        Assert.Contains(
            "class Second",
            context.Syntax.SyntaxTree!.GetText().GetSubText(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void ParseMemberDeclarationResult_RejectsEmptyBodyWithRecoveredSyntax()
    {
        var context = CreateContext(string.Empty);

        var result = context.ParseMemberDeclarationResult();

        Assert.True(result.HasErrors);
        Assert.IsType<IncompleteMemberDeclarationSyntax>(result.Syntax);
        Assert.Contains(result.Diagnostics, static diagnostic => diagnostic.Descriptor.Id == "RAVM022");
    }

    [Fact]
    public void ParseMemberDeclarationResult_RejectsGlobalStatement()
    {
        var context = CreateContext("let value = 1");

        var result = context.ParseMemberDeclarationResult();

        Assert.True(result.HasErrors);
        Assert.IsType<IncompleteMemberDeclarationSyntax>(result.Syntax);
        Assert.Contains(result.Diagnostics, static diagnostic => diagnostic.Descriptor.Id == "RAVM022");
    }

    [Theory]
    [InlineData("import System.*")]
    [InlineData("alias Text = System.String")]
    [InlineData("[assembly: RavenCompilerPlugin]")]
    public void ParseMemberDeclarationResult_RejectsCompilationUnitContent(string body)
    {
        var context = CreateContext(body);

        var result = context.ParseMemberDeclarationResult();

        Assert.True(result.HasErrors);
        Assert.IsType<IncompleteMemberDeclarationSyntax>(result.Syntax);
        Assert.Contains(result.Diagnostics, static diagnostic => diagnostic.Descriptor.Id == "RAVM022");
    }

    [Fact]
    public void CreateUniqueName_AvoidsAuthoredAndPreviouslyGeneratedNames()
    {
        var context = CreateContext("let __macro_item_0 = 0");

        var first = context.CreateUniqueName("item");
        var second = context.CreateUniqueName("item");

        Assert.Equal("__macro_item_1", first);
        Assert.Equal("__macro_item_2", second);
    }

    [Theory]
    [InlineData("item value", "__macro_item_value_0")]
    [InlineData("", "__macro_value_0")]
    [InlineData("$$", "__macro_value_0")]
    public void CreateUniqueName_NormalizesHints(string hint, string expected)
    {
        var context = CreateContext(string.Empty);

        Assert.Equal(expected, context.CreateUniqueName(hint));
    }

    private static TokenTreeMacroContext CreateContext(string body)
    {
        var tree = SyntaxTree.ParseText($"func Main() -> unit => probe! {{ {body} }}");
        var compilation = Compilation.Create(
                "MacroFragmentParsing",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        return new TokenTreeMacroContext(
            compilation,
            compilation.GetSemanticModel(tree),
            invocation);
    }
}
