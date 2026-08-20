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

public sealed class InvocableMacroCodeGenTests
{
    [Fact]
    public void InvocableMacro_ExpandedStatement_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.*

            class Harness {
                public static func Run() -> int {
                    var result = 0
                    setAnswer!{ }
                    return result
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(SetAnswerMacro)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly.GetType("Harness", true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, method!.Invoke(null, null));
    }

    [Fact]
    public void InvocableMacro_WrongExpansionCategory_ReportsDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.*

            func Run() {
                raven!{ 20 + 22 }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(RavenBodyMacro)));

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM022"));

        Assert.Contains("statement syntax is required", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void InvocableMacro_MemberListInExpressionPosition_ReportsDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.*

            func Run() -> int => members!{ }
            """);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(MemberListMacro)));

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM022"));

        Assert.Contains(
            "member-list syntax where expression syntax is required",
            diagnostic.GetMessage(),
            StringComparison.Ordinal);
    }

    [Fact]
    public void InvocableMacro_TypeMemberExpansion_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.*

            class Harness {
                generatedMembers!{ }
            }
            """);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(GeneratedMembersMacro)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", throwOnError: true)!
            .GetMethod("Generated", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, method!.Invoke(null, null));
    }

    [Fact]
    public void InvocableMacro_FileMemberExpansion_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.*

            generatedNamespaceMembers!{ }

            class Harness {
                public static func Run() -> int => Generated.Value()
            }
            """);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(GeneratedNamespaceMembersMacro)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", throwOnError: true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, method!.Invoke(null, null));
    }

    [Fact]
    public void InvocableMacro_NamespaceMemberExpansion_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.*

            namespace Example {
                generatedNamespaceMembers!{ }

                class Harness {
                    public static func Run() -> int => Generated.Value()
                }
            }
            """);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(GeneratedNamespaceMembersMacro)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Example.Harness", throwOnError: true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, method!.Invoke(null, null));
    }

    [Fact]
    public void CompactMacro_MemberListExpansion_IsEmitted()
    {
        var sourceTree = SyntaxTree.ParseText(
            """
            import Raven.CodeAnalysis.Macros.*
            import Raven.CodeAnalysis.Syntax.*

            macro Generate(context: TokenTreeMacroContext) -> SyntaxList<MemberDeclarationSyntax> {
                expand context.ParseCompilationUnit().Members
            }

            Generate! {
                class Generated {
                    static func Value() -> int => 20
                }
            }

            class Harness {
                Generate! {
                    class Nested {
                        static func Value() -> int => 22
                    }
                }

                static func Run() -> int => Generated.Value() + Nested.Value()
            }

            func Main() {}
            """,
            path: "main.rvn");
        var compilation = Compilation.Create(
                "CompactMemberMacroConsumer",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(sourceTree);

        var macro = Assert.Single(
            compilation.GetMacroRegistry().GetMacros(MacroKind.Invocable),
            static macro => macro.Name == "Generate");
        Assert.Equal(
            MacroInvocationTargets.NamespaceMember | MacroInvocationTargets.TypeMember,
            MacroFacts.GetInvocationTargets(macro));
        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", throwOnError: true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, method!.Invoke(null, null));
    }

    [Fact]
    public void LocalMacroSyntaxTrees_ExpandButAreNotEmittedIntoConsumerAssembly()
    {
        var macroTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Macros.*
            import Raven.Macros.*

            class LocalAnswerMacro : ITokenTreeMacro {
                val Name: string => "localAnswer"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    FreestandingMacroExpansionResult {
                        Expression = quote!{ 42 }
                    }
                }
            }
            """);
        var consumerTree = SyntaxTree.ParseText("""
            class Harness {
                public static func Run() -> int => localAnswer!{ }
            }
            """);
        var compilation = Compilation.Create(
                "LocalMacroConsumer",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddMacroSyntaxTrees(macroTree)
            .AddSyntaxTrees(consumerTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", throwOnError: true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, method!.Invoke(null, null));
        Assert.Null(loaded.Assembly.GetType("LocalAnswerMacro"));
    }

    [Fact]
    public void InvocableMacro_ExpandedExpression_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.*

            class Harness {
                public static func Run() -> int {
                    return add!(20, Right: 22)
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(AddMacro)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var method = assembly.GetType("Harness", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, method!.Invoke(null, null));
    }

    [Fact]
    public void InvocableMacro_LoadsFileContentDuringExpansion()
    {
        var path = Path.GetTempFileName();
        try
        {
            const string expected = "embedded at compile time\nwith a second line";
            File.WriteAllText(path, expected);
            var syntaxTree = SyntaxTree.ParseText($$"""
                import Raven.CodeAnalysis.Tests.*

                class Harness {
                    public static func Run() -> string {
                        return embedText!("{{EscapeRavenString(path)}}")
                    }
                }
                """);

            var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddSyntaxTrees(syntaxTree)
                .AddReferences(TestMetadataReferences.Default)
                .AddMacroReferences(new MacroReference(typeof(EmbedTextMacro)));

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
    public void InvocableMacro_MissingFileReportsArgumentDiagnostic()
    {
        var path = Path.Combine(Path.GetTempPath(), Guid.NewGuid() + ".txt");
        var syntaxTree = SyntaxTree.ParseText($$"""
            import Raven.CodeAnalysis.Tests.*

            func Main() -> string => embedText!("{{EscapeRavenString(path)}}")
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(EmbedTextMacro)));

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
            import Raven.CodeAnalysis.Tests.*

            class Harness {
                public static func Run() -> int {
                    return raven!{
                        20 + 22
                    }
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(RavenBodyMacro)));

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
            import Raven.CodeAnalysis.Tests.*

            class Harness {
                public static func Run(value: int) -> bool {
                    return guard!{
                        unless value > 0
                    }
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(GuardMacro)));

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
            import Raven.CodeAnalysis.Tests.*

            class Harness {
                public static func Run(value: int) -> string {
                    return choose!{
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
            .AddMacroReferences(new MacroReference(typeof(ChooseMacro)));

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
            import Raven.CodeAnalysis.Tests.*

            func Main() -> string => choose!{
                test true
                then "yes"
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(ChooseMacro)));

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM021"));

        Assert.Contains("CHOOSE001: Expected an 'otherwise' clause.", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Equal("then", syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    public sealed class AddMacro : IInvocableMacro<AddMacroParameters>
    {
        public string Name => "add";
        public MacroKind Kind => MacroKind.Invocable;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext<AddMacroParameters> context)
            => new()
            {
                Expression = ParseExpression($"{context.Parameters.Left} + {context.Parameters.Right}")
            };
    }

    public sealed class SetAnswerMacro : ITokenTreeMacro
    {
        public string Name => "setAnswer";
        public MacroKind Kind => MacroKind.Invocable;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.FromStatement(
                SyntaxFactory.ParseStatement("result = 42"));
    }

    public sealed class AddMacroParameters(int left)
    {
        public int Left { get; } = left;

        public int Right { get; set; }
    }

    public sealed class EmbedTextMacro : IInvocableMacro<EmbedTextMacroParameters>
    {
        public string Name => "embedText";
        public MacroKind Kind => MacroKind.Invocable;

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

    public sealed class RavenBodyMacro : ITokenTreeMacro
    {
        public string Name => "raven";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => new()
            {
                Expression = context.ParseExpression()
            };
    }

    public sealed class MemberListMacro : ITokenTreeMacro
    {
        public string Name => "members";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var member = SyntaxFactory.ParseSyntaxTree("class Generated {}").GetRoot().Members.Single();
            return FreestandingMacroExpansionResult.FromMembers(
                SyntaxFactory.SingletonList(member));
        }
    }

    public sealed class GeneratedMembersMacro : ITokenTreeMacro
    {
        public string Name => "generatedMembers";
        public MacroInvocationTargets InvocationTargets => MacroInvocationTargets.TypeMember;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var container = Assert.IsType<ClassDeclarationSyntax>(
                SyntaxFactory.ParseSyntaxTree("""
                    class __GeneratedContainer {
                        public static func Generated() -> int => 42
                    }
                    """).GetRoot().Members.Single());
            return FreestandingMacroExpansionResult.FromMembers(container.Members);
        }
    }

    public sealed class GeneratedNamespaceMembersMacro : ITokenTreeMacro
    {
        public string Name => "generatedNamespaceMembers";
        public MacroInvocationTargets InvocationTargets => MacroInvocationTargets.NamespaceMember;

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
        {
            var member = SyntaxFactory.ParseSyntaxTree("""
                class Generated {
                    static func Value() -> int => 42
                }
                """).GetRoot().Members.Single();
            return FreestandingMacroExpansionResult.FromMembers(
                SyntaxFactory.SingletonList(member));
        }
    }

    public sealed class GuardMacro : ITokenTreeMacro, IMacroKeywordProvider
    {
        private const int UnlessKeywordRawKind = 80_001;

        public string Name => "guard";

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

    public sealed class ChooseMacro : ITokenTreeMacro, IMacroKeywordProvider
    {
        private const int TestKeywordRawKind = 80_002;
        private const int ThenKeywordRawKind = 80_003;
        private const int OtherwiseKeywordRawKind = 80_004;

        public string Name => "choose";

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
