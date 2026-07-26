using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class QueryMacroCodeGenTests
{
    [Fact]
    public void QueryMacro_FromWhereSelect_BindsRangeVariableAndEmits()
    {
        var result = InvokeRun("""
            import System.Linq.*

            class Harness {
                public static func Run() -> int {
                    let value = 100
                    let query = #query {
                        from value in [1, 2, 3, 4]
                        where value > 2
                        select value * 10
                    }

                    return value + query.Sum()
                }
            }
            """);

        Assert.Equal(170, result);
    }

    [Fact]
    public void QueryMacro_FromSelectWithoutWhere_Emits()
    {
        var result = InvokeRun("""
            import System.Linq.*

            class Harness {
                public static func Run() -> int {
                    let query = #query {
                        from value in [1, 2, 3]
                        select value + 1
                    }

                    return query.Sum()
                }
            }
            """);

        Assert.Equal(9, result);
    }

    [Fact]
    public void QueryMacro_MissingSelectClause_ReportsBodyDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            func Main() -> int => #query {
                from value in [1, 2, 3]
                where value > 1
            }
            """);

        var compilation = CreateCompilation(syntaxTree);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM021"));

        Assert.Contains("QUERY001: Expected a 'select' clause.", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Equal("where", syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void QueryMacro_MalformedEmbeddedExpression_ReportsParserDiagnosticAtAuthoredLocation()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            func Main() -> int => #query {
                from value in [1, 2, 3]
                where value.Equals(1, )
                select value
            }
            """);

        var compilation = CreateCompilation(syntaxTree);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAV1525"));

        Assert.Same(syntaxTree, diagnostic.Location.SourceTree);
        Assert.Equal(")", syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    private static object? InvokeRun(string source)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        return method!.Invoke(null, null);
    }

    private static Compilation CreateCompilation(SyntaxTree syntaxTree)
        => Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(QueryMacroPlugin)));

    public sealed class QueryMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(QueryMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new QueryMacro()];
    }

    public sealed class QueryMacro : ITokenTreeExpressionMacro, IMacroKeywordProvider
    {
        private const int FromKeywordRawKind = 80_005;
        private const int SelectKeywordRawKind = 80_006;

        public string Name => "query";
        public MacroTarget Targets => MacroTarget.None;

        public ImmutableArray<MacroKeyword> Keywords =>
        [
            new("from", FromKeywordRawKind, MacroKeywordClassification.ReservedWord),
            new("select", SelectKeywordRawKind, MacroKeywordClassification.ReservedWord)
        ];

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var stream = context.CreateTokenStream();
            if (stream.IsEndOfFile)
                return Error(context, new TextSpan(0, 0), "Expected a 'from' clause.");

            var fromKeyword = stream.ReadToken();
            if (fromKeyword.RawKind != FromKeywordRawKind)
                return Error(context, fromKeyword.Span, "Expected the 'from' keyword.");

            if (stream.IsEndOfFile)
                return Error(context, fromKeyword.Span, "Expected a range variable after 'from'.");

            var rangeVariable = stream.ReadToken();
            if (rangeVariable.Kind != SyntaxKind.IdentifierToken)
                return Error(context, rangeVariable.Span, "Expected a range-variable identifier.");

            if (stream.IsEndOfFile)
                return Error(context, rangeVariable.Span, "Expected 'in' after the range variable.");

            var inKeyword = stream.ReadToken();
            if (inKeyword.Kind != SyntaxKind.InKeyword)
                return Error(context, inKeyword.Span, "Expected the 'in' keyword.");

            var whereKeyword = inKeyword;
            var selectKeyword = inKeyword;

            while (!stream.IsEndOfFile)
            {
                var token = stream.ReadToken();
                if (token.RawKind == SelectKeywordRawKind)
                {
                    selectKeyword = token;
                    break;
                }

                if (token.Kind == SyntaxKind.WhereKeyword && whereKeyword.Kind != SyntaxKind.WhereKeyword)
                    whereKeyword = token;
            }

            if (selectKeyword.RawKind != SelectKeywordRawKind)
            {
                var diagnosticToken = whereKeyword.Kind == SyntaxKind.WhereKeyword
                    ? whereKeyword
                    : inKeyword;
                return Error(context, diagnosticToken.Span, "Expected a 'select' clause.");
            }

            var sourceEnd = whereKeyword.Kind == SyntaxKind.WhereKeyword
                ? whereKeyword.SpanStart
                : selectKeyword.SpanStart;
            var sourceResult = context.ParseExpressionResult(
                TextSpan.FromBounds(inKeyword.Span.End, sourceEnd));
            ExpressionSyntax query = sourceResult.Syntax;
            var diagnostics = sourceResult.Diagnostics;

            if (whereKeyword.Kind == SyntaxKind.WhereKeyword)
            {
                var predicateResult = context.ParseExpressionResult(
                    TextSpan.FromBounds(whereKeyword.Span.End, selectKeyword.SpanStart));
                diagnostics = diagnostics.AddRange(predicateResult.Diagnostics);
                query = InvokeQueryOperator(
                    query,
                    "Where",
                    CreateLambda(rangeVariable.ValueText, predicateResult.Syntax));
            }

            var selectorResult = context.ParseExpressionResult(
                TextSpan.FromBounds(selectKeyword.Span.End, context.BodySpan.Length));
            diagnostics = diagnostics.AddRange(selectorResult.Diagnostics);
            query = InvokeQueryOperator(
                query,
                "Select",
                CreateLambda(rangeVariable.ValueText, selectorResult.Syntax));

            return FreestandingMacroExpansionResult.FromExpression(query, diagnostics);
        }

        private static ExpressionSyntax CreateLambda(string parameterName, ExpressionSyntax expression)
            => SyntaxFactory.SimpleFunctionExpression(
                SyntaxFactory.Token(SyntaxKind.None),
                SyntaxFactory.Token(SyntaxKind.None),
                SyntaxFactory.Token(SyntaxKind.None),
                SyntaxFactory.Parameter(
                    SyntaxFactory.List<AttributeListSyntax>(),
                    SyntaxFactory.Identifier(parameterName)),
                returnType: null,
                body: null,
                expressionBody: SyntaxFactory.ArrowExpressionClause(expression));

        private static ExpressionSyntax InvokeQueryOperator(
            ExpressionSyntax source,
            string methodName,
            ExpressionSyntax argument)
            => SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    source,
                    SyntaxFactory.DotToken,
                    SyntaxFactory.IdentifierName(methodName)),
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                    [
                        new SyntaxNodeOrToken(SyntaxFactory.Argument(argument))
                    ])));

        private static FreestandingMacroExpansionResult Error(
            TokenTreeMacroContext context,
            TextSpan span,
            string message)
            => FreestandingMacroExpansionResult.FromDiagnostic(
                context.CreateBodyDiagnostic(span, message, code: "QUERY001"));
    }
}
