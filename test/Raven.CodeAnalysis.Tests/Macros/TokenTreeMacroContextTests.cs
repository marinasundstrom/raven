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

        Assert.False(result.HasErrors, string.Join(Environment.NewLine, result.Diagnostics));
        Assert.IsType<InfixOperatorExpressionSyntax>(result.Syntax);
        Assert.Equal(context.BodySpan.Start + start, result.Syntax.Span.Start);
    }

    [Fact]
    public void WithOrigin_UsesParsedFragmentSpan()
    {
        var context = CreateContext("prefix value + 1");
        var sourceSpan = BodySpanOf(context, "value + 1");
        var source = context.ParseExpressionResult(sourceSpan);
        var generated = SyntaxFactory.ParseExpression("Build()")!;

        var mapped = context.WithOrigin(generated, source);

        Assert.True(MacroSyntaxOrigin.TryGetSourceSpan(
            mapped,
            context.Syntax.SyntaxTree!,
            out var mappedSpan));
        Assert.Equal(
            new TextSpan(context.BodySpan.Start + sourceSpan.Start, sourceSpan.Length),
            mappedSpan);
    }

    [Fact]
    public void MacroSyntax_ProvidesStructuralAndFactoryForms()
    {
        var syntax = SyntaxFactory.ParseExpression("left + right")!;

        var structure = MacroSyntax.GetStructure(syntax);
        var factory = MacroSyntax.GetFactoryForm(syntax);

        Assert.Contains("AddExpression", structure);
        Assert.Contains("SyntaxFactory", factory);
        Assert.Contains("IdentifierName", factory);
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
        Assert.Equal(span, result.BodyRelativeSpan);
        Assert.Equal(context.BodySpan.Start + start, result.Syntax.Span.Start);
    }

    [Fact]
    public void ParseExpression_ConsumesOneFragmentFromCurrentStreamPosition()
    {
        var context = CreateContext("from 40 + 2 select");
        var stream = context.CreateTokenStream();
        Assert.Equal("from", stream.ReadToken().ValueText);

        var result = stream.ParseExpression();

        Assert.False(result.HasErrors);
        Assert.Equal("40 + 2", result.Syntax.ToString());
        Assert.Equal(BodySpanOf(context, "40 + 2"), result.BodyRelativeSpan);
        Assert.Equal("select", stream.PeekToken().ValueText);
    }

    [Fact]
    public void GetTypeInfo_ResolvesParsedExpressionInInvocationScope()
    {
        var context = CreateContextWithLocal("count");
        var expression = context.ParseExpression();

        var info = context.GetTypeInfo(expression);

        Assert.Equal(SpecialType.System_Int32, info.Type?.SpecialType);
    }

    [Fact]
    public void GetSymbolInfo_ResolvesParsedExpressionInInvocationScope()
    {
        var context = CreateContextWithLocal("count");
        var expression = context.ParseExpression();

        var info = context.GetSymbolInfo(expression);

        var local = Assert.IsAssignableFrom<ILocalSymbol>(info.Symbol);
        Assert.Equal("count", local.Name);
        Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);
    }

    [Fact]
    public void ParseStatement_ConsumesOneFragmentFromCurrentStreamPosition()
    {
        var context = CreateContext("do return 42 then");
        var stream = context.CreateTokenStream();
        Assert.Equal("do", stream.ReadToken().ValueText);

        var result = stream.ParseStatement();

        Assert.False(result.HasErrors);
        Assert.IsType<ReturnStatementSyntax>(result.Syntax);
        Assert.Equal(BodySpanOf(context, "return 42"), result.BodyRelativeSpan);
        Assert.Equal("then", stream.PeekToken().ValueText);
    }

    [Fact]
    public void ParseType_ConsumesOneFragmentFromCurrentStreamPosition()
    {
        var context = CreateContext("as Dictionary<string, int> then");
        var stream = context.CreateTokenStream();
        Assert.Equal("as", stream.ReadToken().ValueText);

        var result = stream.ParseType();

        Assert.False(result.HasErrors);
        Assert.Equal("Dictionary<string, int>", result.Syntax.ToString());
        Assert.Equal(BodySpanOf(context, "Dictionary<string, int>"), result.BodyRelativeSpan);
        Assert.Equal("then", stream.PeekToken().ValueText);
    }

    [Fact]
    public void ParsePattern_ConsumesOneFragmentFromCurrentStreamPosition()
    {
        var context = CreateContext("case let value then");
        var stream = context.CreateTokenStream();
        Assert.Equal("case", stream.ReadToken().ValueText);

        var result = stream.ParsePattern();

        Assert.False(result.HasErrors);
        Assert.IsType<VariablePatternSyntax>(result.Syntax);
        Assert.Equal(BodySpanOf(context, "let value"), result.BodyRelativeSpan);
        Assert.Equal("then", stream.PeekToken().ValueText);
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
    public void ParseMemberDeclaration_ConsumesOneDeclarationFromCurrentStreamPosition()
    {
        var context = CreateContext("members class First { }\nclass Second { }");
        var stream = context.CreateTokenStream();
        Assert.Equal("members", stream.ReadToken().ValueText);

        var result = stream.ParseMemberDeclaration();

        Assert.False(result.HasErrors, string.Join(Environment.NewLine, result.Diagnostics));
        Assert.Equal("First", Assert.IsType<ClassDeclarationSyntax>(result.Syntax).Identifier.ValueText);
        Assert.Equal("class", stream.PeekToken().ValueText);
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

    [Fact]
    public void RequireSyntax_ReturnsMatchingNodeWithoutDiagnostic()
    {
        var context = CreateContext("value + 1");
        var expression = context.ParseExpression();

        var required = context.RequireSyntax<ExpressionSyntax>(expression);

        Assert.Same(expression, required);
        Assert.Empty(context.GetReportedMacroDiagnostics());
    }

    [Fact]
    public void RequireSyntax_ReportsMismatchAtAuthoredNode()
    {
        var context = CreateContext("class Widget { }");
        var compilationUnit = context.ParseCompilationUnit();

        var required = context.RequireSyntax<ExpressionSyntax>(compilationUnit);

        Assert.Null(required);
        var diagnostic = Assert.Single(context.GetReportedMacroDiagnostics());
        Assert.Equal("Expected ExpressionSyntax, but found CompilationUnit.", diagnostic.Message);
        Assert.Equal(compilationUnit.Span, diagnostic.Location!.SourceSpan);
    }

    [Fact]
    public void RequireSyntax_UsesInvocationForDetachedSyntax()
    {
        var context = CreateContext(string.Empty);
        var detached = SyntaxFactory.ParseCompilationUnit("class Widget { }");

        var required = context.RequireSyntax<ExpressionSyntax>(
            detached,
            "An expression is required.",
            "TEST001");

        Assert.Null(required);
        var diagnostic = Assert.Single(context.GetReportedMacroDiagnostics());
        Assert.Equal("An expression is required.", diagnostic.Message);
        Assert.Equal("TEST001", diagnostic.Code);
        Assert.Equal(context.Syntax.TokenTree!.Span, diagnostic.Location!.SourceSpan);
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

    private static TextSpan BodySpanOf(TokenTreeMacroContext context, string text)
    {
        var start = context.GetBodyText().IndexOf(text, StringComparison.Ordinal);
        Assert.True(start >= 0);
        return new TextSpan(start, text.Length);
    }

    private static TokenTreeMacroContext CreateContextWithLocal(string body)
    {
        var tree = SyntaxTree.ParseText($$"""
            func Main() {
                let count = 42
                probe! { {{body}} }
            }
            """);
        var compilation = Compilation.Create(
                "MacroFragmentSemantics",
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
