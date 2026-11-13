using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Tests.Symbols;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Macros.Tests;

[Trait("Target", "Shared")]
public sealed partial class TokenTreeMacroContextTests
{
    [Fact]
    public void CreateTokenStream_ReclassifiesContextualKeywordAsMacroKeyword()
    {
        var context = CreateContext("then value", includeKeyword: true);

        var token = context.CreateTokenStream().ReadToken();

        Assert.Equal(SyntaxKind.ThenKeyword, token.Kind);
        Assert.Equal(ThenRawKind, token.RawKind);
    }

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
    public void ParseProjectedExpressionResult_RetainsAuthoredPosition()
    {
        var context = CreateContext("#(value) + 1");
        var projectedBody = context.GetBodyText().Replace("#(value)", "replaced");

        var result = context.ParseProjectedExpressionResult(projectedBody);

        Assert.False(result.HasErrors, string.Join(Environment.NewLine, result.Diagnostics));
        Assert.Equal("replaced + 1", result.Syntax.ToString());
        Assert.Equal(
            context.BodySpan.Start + projectedBody.IndexOf("replaced", StringComparison.Ordinal),
            result.Syntax.Span.Start);
    }

    [Fact]
    public void ParseProjectedExpressionResult_RejectsChangedLineBreaks()
    {
        var context = CreateContext("value +\n1");
        var projectedBody = context.GetBodyText().Replace('\n', ' ');

        var exception = Assert.Throws<ArgumentException>(
            () => context.ParseProjectedExpressionResult(projectedBody));

        Assert.Contains("line breaks", exception.Message);
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
    public void ReportDiagnostics_AcceptsParseResult()
    {
        var context = CreateContext("List<");
        var result = context.ParseTypeResult();

        context.ReportDiagnostics(result);

        var reported = Assert.Single(context.GetReportedDiagnostics());
        var parsed = Assert.Single(result.Diagnostics);
        Assert.Equal(parsed.Descriptor.Id, reported.Descriptor.Id);
        Assert.Equal(parsed.Location.SourceSpan, reported.Location.SourceSpan);
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
    public void CreateUniqueIdentifier_ConstructsReusableIdentifierSyntax()
    {
        var context = CreateContext("let __macro_item_0 = 0");

        var identifier = context.CreateUniqueIdentifier("item");

        Assert.Equal("__macro_item_1", identifier.Identifier.ValueText);
        Assert.Equal("__macro_item_1", identifier.ToString());
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
        Assert.Equal(context.TokenTree.Span, diagnostic.Location!.SourceSpan);
    }

    [Fact]
    public void SemanticQueries_UseInvocationScopeAndPreserveServiceResults()
    {
        var services = new RecordingServices();
        var context = CreateContext("value", services: services);
        var expression = context.ParseExpression();

        Assert.Same(services.TypeResult, context.GetTypeInfo(expression));
        Assert.Same(context.Syntax, services.Invocation);
        Assert.Same(expression, services.Expression);
        Assert.Same(services.ResultType, context.GetSymbolInfo(expression).Symbol);
        Assert.Same(context.Syntax, services.Invocation);
        Assert.Same(expression, services.Expression);
        Assert.Equal(2, services.Queries);
    }

    [Fact]
    public void SequenceLocal_UsesErrorTypeWhenExpressionTypeIsUnavailable()
    {
        var services = new RecordingServices();
        var context = CreateContext("value", services: services);
        var local = context.CreateSequenceElementLocal("item", BodySpanOf(context, "value"));

        Assert.Same(services.ResultType, local.Type);
        Assert.Same(context.Syntax, services.Invocation);
        Assert.Equal("value", services.Expression!.ToString());
        Assert.Equal(1, services.Queries);
    }

    [Fact]
    public void SemanticQueries_RejectNullBeforeCallingServices()
    {
        var services = new RecordingServices();
        var context = CreateContext("value", services: services);

        Assert.Throws<ArgumentNullException>(() => context.GetTypeInfo(null!));
        Assert.Throws<ArgumentNullException>(() => context.GetSymbolInfo(null!));
        Assert.Equal(0, services.Queries);
    }

    private sealed class RecordingServices : SyntaxOnlyServices
    {
        public TypeSymbol ResultType { get; } = MetadataSymbolFixture.Named("Services", "Example.Value");
        public TypeInfo TypeResult { get; } = new TypeInfo(null, null);
        public SyntaxNode? Invocation { get; private set; }
        public ExpressionSyntax? Expression { get; private set; }
        public int Queries { get; private set; }
        public override TypeSymbol ErrorType => ResultType;

        private void Record(SyntaxNode invocation, ExpressionSyntax expression)
        {
            Invocation = invocation;
            Expression = expression;
            Queries++;
        }

        public override TypeInfo GetTypeInfo(SyntaxNode invocation, ExpressionSyntax expression)
        {
            Record(invocation, expression);
            return TypeResult;
        }

        public override SymbolInfo GetSymbolInfo(SyntaxNode invocation, ExpressionSyntax expression)
        {
            Record(invocation, expression);
            return new SymbolInfo(ResultType);
        }

        public override TypeSymbol? GetExpressionType(SyntaxNode invocation, ExpressionSyntax expression)
        {
            Record(invocation, expression);
            return null;
        }
    }

    private const int ThenRawKind = 91_001;

    private static TokenTreeMacroContext CreateContext(string body, bool includeKeyword = false, MacroFragmentServices? services = null)
    {
        var tree = SyntaxTree.ParseText($"func Main() -> unit => probe! {{ {body} }}");
        var invocation = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        return new TokenTreeMacroContext(services ?? new SyntaxOnlyServices(), FreestandingMacroInvocation.Create(invocation),
            tokenStreamProvider: null, includeKeyword ? [new MacroKeyword("then", ThenRawKind)] : [], default);
    }

    private static TextSpan BodySpanOf(TokenTreeMacroContext context, string text)
    {
        var start = context.GetBodyText().IndexOf(text, StringComparison.Ordinal);
        Assert.True(start >= 0);
        return new TextSpan(start, text.Length);
    }

    private class SyntaxOnlyServices : MacroFragmentServices, MacroArgumentServices
    {
        public MacroArgumentServices ArgumentServices => this;
        public virtual TypeSymbol ErrorType => throw new InvalidOperationException("Unexpected semantic query");
        public TypeSymbol CharacterType => throw new InvalidOperationException("Unexpected semantic query");
        public NamedTypeSymbol? GetSequenceDefinition(bool asynchronous) => throw new InvalidOperationException("Unexpected semantic query");
        public virtual TypeSymbol? GetExpressionType(SyntaxNode invocation, ExpressionSyntax expression) => throw new InvalidOperationException("Unexpected semantic query");
        public virtual TypeInfo GetTypeInfo(SyntaxNode invocation, ExpressionSyntax expression) => throw new InvalidOperationException("Unexpected semantic query");
        public virtual SymbolInfo GetSymbolInfo(SyntaxNode invocation, ExpressionSyntax expression) => throw new InvalidOperationException("Unexpected semantic query");
        public TypeSymbol GetSpecialType(SpecialType specialType) => throw new InvalidOperationException("Unexpected semantic query");
        public TypeSymbol? GetArgumentType(ExpressionSyntax expression) => throw new InvalidOperationException("Unexpected semantic query");
        public TypeSymbol? ResolveConstraint(Type runtimeType) => throw new InvalidOperationException("Unexpected semantic query");
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => throw new InvalidOperationException("Unexpected semantic query");
    }

}
