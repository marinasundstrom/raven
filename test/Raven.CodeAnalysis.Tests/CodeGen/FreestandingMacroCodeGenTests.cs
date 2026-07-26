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

public sealed class FreestandingMacroCodeGenTests
{
    [Fact]
    public void FreestandingMacro_ExpandedExpression_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            class Harness {
                public static func Run() -> int {
                    return #add(20, Right: 22)
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(AddMacroPlugin)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var method = assembly.GetType("Harness", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, method!.Invoke(null, null));
    }

    [Fact]
    public void FreestandingMacro_LoadsFileContentDuringExpansion()
    {
        var path = Path.GetTempFileName();
        try
        {
            const string expected = "embedded at compile time\nwith a second line";
            File.WriteAllText(path, expected);
            var syntaxTree = SyntaxTree.ParseText($$"""
                class Harness {
                    public static func Run() -> string {
                        return #embedText("{{EscapeRavenString(path)}}")
                    }
                }
                """);

            var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddSyntaxTrees(syntaxTree)
                .AddReferences(TestMetadataReferences.Default)
                .AddMacroReferences(new MacroReference(typeof(EmbedTextMacroPlugin)));

            using var peStream = new MemoryStream();
            var result = compilation.Emit(peStream);
            Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

            using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
            var method = loaded.Assembly
                .GetType("Harness", true)!
                .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

            Assert.Equal(expected, method!.Invoke(null, null));
        }
        finally
        {
            File.Delete(path);
        }
    }

    [Fact]
    public void FreestandingMacro_MissingFileReportsArgumentDiagnostic()
    {
        var path = Path.Combine(Path.GetTempPath(), Guid.NewGuid() + ".txt");
        var syntaxTree = SyntaxTree.ParseText($$"""
            func Main() -> string => #embedText("{{EscapeRavenString(path)}}")
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(EmbedTextMacroPlugin)));

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM021"));

        Assert.Contains("EMBED001: Could not read", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Equal(
            $"\"{EscapeRavenString(path)}\"",
            syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void TokenTreeMacro_ParsedRavenExpression_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            class Harness {
                public static func Run() -> int {
                    return #raven {
                        20 + 22
                    }
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(TokenTreeMacroPlugin)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, method!.Invoke(null, null));
    }

    [Fact]
    public void TokenTreeMacro_KeywordDslWithEmbeddedRavenExpression_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            class Harness {
                public static func Run(value: int) -> bool {
                    return #guard {
                        unless value > 0
                    }
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(GuardMacroPlugin)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(false, method!.Invoke(null, [1]));
        Assert.Equal(true, method.Invoke(null, [0]));
    }

    [Fact]
    public void TokenTreeMacro_MultipleKeywordClausesAndRavenExpressions_AreEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            class Harness {
                public static func Run(value: int) -> string {
                    return #choose {
                        test value > 0
                        then "positive"
                        otherwise "not positive"
                    }
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(ChooseMacroPlugin)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal("positive", method!.Invoke(null, [1]));
        Assert.Equal("not positive", method.Invoke(null, [0]));
    }

    [Fact]
    public void TokenTreeMacro_MissingClause_ReportsBodyDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            func Main() -> string => #choose {
                test true
                then "yes"
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(ChooseMacroPlugin)));

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM021"));

        Assert.Contains("CHOOSE001: Expected an 'otherwise' clause.", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Equal("then", syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    public sealed class AddMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(AddMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new AddMacro()];
    }

    public sealed class AddMacro : IFreestandingExpressionMacro<AddMacroParameters>
    {
        public string Name => "add";
        public MacroKind Kind => MacroKind.FreestandingExpression;
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext<AddMacroParameters> context)
            => new()
            {
                Expression = ParseExpression($"{context.Parameters.Left} + {context.Parameters.Right}")
            };
    }

    public sealed class AddMacroParameters(int left)
    {
        public int Left { get; } = left;

        public int Right { get; set; }
    }

    public sealed class EmbedTextMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(EmbedTextMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new EmbedTextMacro()];
    }

    public sealed class EmbedTextMacro : IFreestandingExpressionMacro<EmbedTextMacroParameters>
    {
        public string Name => "embedText";
        public MacroKind Kind => MacroKind.FreestandingExpression;
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(
            FreestandingMacroContext<EmbedTextMacroParameters> context)
        {
            context.CancellationToken.ThrowIfCancellationRequested();

            try
            {
                var content = File.ReadAllText(context.Parameters.Path);
                return new FreestandingMacroExpansionResult
                {
                    Expression = SyntaxFactory.LiteralExpression(
                        SyntaxKind.StringLiteralExpression,
                        SyntaxFactory.Literal(
                            $"\"{EscapeRavenString(content)}\"",
                            content))
                };
            }
            catch (Exception exception) when (
                exception is IOException or UnauthorizedAccessException)
            {
                return new FreestandingMacroExpansionResult
                {
                    MacroDiagnostics =
                    [
                        context.CreateArgumentDiagnostic(
                            context.Arguments[0],
                            $"Could not read '{context.Parameters.Path}': {exception.Message}",
                            code: "EMBED001")
                    ]
                };
            }
        }
    }

    public sealed class EmbedTextMacroParameters(string path)
    {
        public string Path { get; } = path;
    }

    public sealed class TokenTreeMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(TokenTreeMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new RavenBodyMacro()];
    }

    public sealed class RavenBodyMacro : ITokenTreeExpressionMacro
    {
        public string Name => "raven";
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => new()
            {
                Expression = context.ParseExpression()
            };
    }

    public sealed class GuardMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(GuardMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new GuardMacro()];
    }

    public sealed class GuardMacro : ITokenTreeExpressionMacro, IMacroKeywordProvider
    {
        private const int UnlessKeywordRawKind = 80_001;

        public string Name => "guard";
        public MacroTarget Targets => MacroTarget.None;

        public ImmutableArray<MacroKeyword> Keywords =>
        [
            new("unless", UnlessKeywordRawKind)
        ];

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var stream = context.CreateTokenStream();
            if (stream.IsEndOfFile)
                return Error(context, new TextSpan(0, 0), "Expected 'unless' followed by a Raven expression.");

            var keyword = stream.ReadToken();
            if (keyword.RawKind != UnlessKeywordRawKind)
                return Error(context, keyword.Span, "Expected the 'unless' keyword.");

            if (stream.IsEndOfFile)
                return Error(context, keyword.Span, "Expected a Raven expression after 'unless'.");

            var condition = context.ParseExpression(
                TextSpan.FromBounds(keyword.Span.End, context.BodySpan.Length));

            return new FreestandingMacroExpansionResult
            {
                Expression = SyntaxFactory.PrefixOperatorExpression(
                    SyntaxKind.LogicalNotExpression,
                    SyntaxFactory.ExclamationToken,
                    condition)
            };
        }

        private static FreestandingMacroExpansionResult Error(
            TokenTreeMacroContext context,
            TextSpan span,
            string message)
            => new()
            {
                MacroDiagnostics =
                [
                    context.CreateBodyDiagnostic(span, message, code: "GUARD001")
                ]
            };
    }

    public sealed class ChooseMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(ChooseMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ChooseMacro()];
    }

    public sealed class ChooseMacro : ITokenTreeExpressionMacro, IMacroKeywordProvider
    {
        private const int TestKeywordRawKind = 80_002;
        private const int ThenKeywordRawKind = 80_003;
        private const int OtherwiseKeywordRawKind = 80_004;

        public string Name => "choose";
        public MacroTarget Targets => MacroTarget.None;

        public ImmutableArray<MacroKeyword> Keywords =>
        [
            new("test", TestKeywordRawKind, MacroKeywordClassification.ReservedWord),
            new("then", ThenKeywordRawKind, MacroKeywordClassification.ReservedWord),
            new("otherwise", OtherwiseKeywordRawKind, MacroKeywordClassification.ReservedWord)
        ];

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var stream = context.CreateTokenStream();
            if (stream.IsEndOfFile)
                return Error(context, new TextSpan(0, 0), "Expected a 'test' clause.");

            var testKeyword = stream.ReadToken();
            if (testKeyword.RawKind != TestKeywordRawKind)
                return Error(context, testKeyword.Span, "Expected the 'test' keyword.");

            if (!TryReadUntil(stream, ThenKeywordRawKind, out var thenKeyword))
                return Error(context, testKeyword.Span, "Expected a 'then' clause.");

            if (!TryReadUntil(stream, OtherwiseKeywordRawKind, out var otherwiseKeyword))
                return Error(context, thenKeyword.Span, "Expected an 'otherwise' clause.");

            var condition = context.ParseExpression(
                TextSpan.FromBounds(testKeyword.Span.End, thenKeyword.SpanStart));
            var whenTrue = context.ParseExpression(
                TextSpan.FromBounds(thenKeyword.Span.End, otherwiseKeyword.SpanStart));
            var whenFalse = context.ParseExpression(
                TextSpan.FromBounds(otherwiseKeyword.Span.End, context.BodySpan.Length));

            return new FreestandingMacroExpansionResult
            {
                Expression = SyntaxFactory.IfExpression(
                    SyntaxFactory.IfKeyword,
                    condition,
                    whenTrue,
                    SyntaxFactory.ElseExpressionClause(SyntaxFactory.ElseKeyword, whenFalse))
            };
        }

        private static bool TryReadUntil(
            IMacroTokenStream stream,
            int rawKind,
            out SyntaxToken result)
        {
            while (!stream.IsEndOfFile)
            {
                var token = stream.ReadToken();
                if (token.RawKind == rawKind)
                {
                    result = token;
                    return true;
                }
            }

            result = default;
            return false;
        }

        private static FreestandingMacroExpansionResult Error(
            TokenTreeMacroContext context,
            TextSpan span,
            string message)
            => new()
            {
                MacroDiagnostics =
                [
                    context.CreateBodyDiagnostic(span, message, code: "CHOOSE001")
                ]
            };
    }

    private static ExpressionSyntax ParseExpression(string expressionText)
    {
        var tree = SyntaxTree.ParseText($$"""
            func Main() -> int => {{expressionText}}
            """);

        return tree.GetRoot()
            .DescendantNodes()
            .OfType<ArrowExpressionClauseSyntax>()
            .Single()
            .Expression;
    }

    private static string EscapeRavenString(string text)
        => text
            .Replace("\\", "\\\\", StringComparison.Ordinal)
            .Replace("\"", "\\\"", StringComparison.Ordinal)
            .Replace("\r", "\\r", StringComparison.Ordinal)
            .Replace("\n", "\\n", StringComparison.Ordinal)
            .Replace("\t", "\\t", StringComparison.Ordinal);
}
