using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public sealed class CompletionServiceMacroTests
{
    [Fact]
    public void GetCompletions_InsideReportedExpressionFragment_UsesCallerScope()
    {
        const string code = """
class MacroHost {
    func Test() {
        let message = "hello"
        let value = fragment!{ message. }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new FragmentMacro()));

        var position = code.IndexOf("message.", StringComparison.Ordinal) + "message.".Length;
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        Assert.Contains(items, static item => item.DisplayText == "Length");
    }

    [Fact]
    public void GetCompletions_InsideReportedExpressionFragment_ReturnsCallerSymbols()
    {
        const string code = """
class MacroHost {
    func Test() {
        let message = "hello"
        let value = fragment!{ mes }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new FragmentMacro()));

        var position = code.IndexOf("mes }", StringComparison.Ordinal) + "mes".Length;
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        var message = Assert.Single(items.Where(static item => item.DisplayText == "message"));
        Assert.Equal("mes", code.Substring(message.ReplacementSpan.Start, message.ReplacementSpan.Length));
    }

    [Fact]
    public void GetCompletions_InsideMacroFragmentContribution_UsesCallerScope()
    {
        const string code = """
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Text.*

macro RavenExpression(context: TokenTreeMacroContext) {
    let span = TextSpan(0, context.BodySpan.Length)
    fragment context.CreateFragmentRegion(MacroFragmentKind.Expression, span)
    expand context.ParseExpression(span)
}

class MacroHost {
    func Test() {
        let message = "hello"
        let value = RavenExpression! { mes }
    }
}
""";
        var authoredTree = SyntaxTree.ParseText(code, path: "main.rvn");
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(authoredTree);
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var position = code.LastIndexOf("mes }", StringComparison.Ordinal) + "mes".Length;

        var items = new CompletionService()
            .GetCompletions(compilation, consumerTree, position)
            .ToList();

        Assert.Contains(items, static item => item.DisplayText == "message");
    }

    [Fact]
    public void GetCompletions_AtReportedEmptyExpressionFragment_ReturnsCallerSymbols()
    {
        const string code = """
class MacroHost {
    func Test() {
        let message = "hello"
        let value = emptyFragment!{ }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new EmptyFragmentMacro()));

        var position = code.IndexOf("}", code.IndexOf("emptyFragment!", StringComparison.Ordinal), StringComparison.Ordinal);
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        Assert.Contains(items, static item => item.DisplayText == "message");
    }

    [Fact]
    public void GetCompletions_InsideMacroOwnedDsl_UsesProviderAndMapsBodyRelativeSpan()
    {
        const string code = """
class MacroHost {
    func Test() {
        let value = completionDsl!{ <Wid }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new CompletionDslMacro()));

        var position = code.IndexOf("Wid }", StringComparison.Ordinal) + "Wid".Length;
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        var widget = Assert.Single(items);
        Assert.Equal("Widget", widget.DisplayText);
        Assert.Equal("Widget", widget.InsertionText);
        Assert.Equal("Wid", code.Substring(widget.ReplacementSpan.Start, widget.ReplacementSpan.Length));
        Assert.Equal("macro DSL item", widget.Description);
    }

    [Fact]
    public async Task GetCompletionsAsync_PropagatesCancellationTokenToMacroProvider()
    {
        const string code = "let value = completionDsl!{ <Wid }";
        var syntaxTree = SyntaxTree.ParseText(code);
        var macro = new CompletionDslMacro();
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(macro));
        var position = code.IndexOf("Wid", StringComparison.Ordinal) + "Wid".Length;
        using var cancellation = new CancellationTokenSource();

        var items = await new CompletionService()
            .GetCompletionsAsync(compilation, syntaxTree, position, cancellation.Token);

        Assert.Contains(items, static item => item.DisplayText == "Widget");
        Assert.True(macro.SawCancellableToken);
    }

    [Fact]
    public void GetCompletions_InsideNestedMacroWithoutRavenFragment_UsesNestedProvider()
    {
        const string code = """
class MacroHost {
    func Test() {
        let value = outerBlock! { completionDsl!{ <Wid } }
    }
}
""";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(
                new MacroReference(new OuterBlockMacro()),
                new MacroReference(new CompletionDslMacro()));
        var position = code.IndexOf("Wid", StringComparison.Ordinal) + "Wid".Length;

        var widget = Assert.Single(
            new CompletionService().GetCompletions(compilation, syntaxTree, position),
            static item => item.DisplayText == "Widget");

        Assert.Equal("Wid", code.Substring(widget.ReplacementSpan.Start, widget.ReplacementSpan.Length));
    }

    [Fact]
    public void GetCompletions_InsideIndependentBlockFragments_UsesEachReportedSpan()
    {
        const string code = """
class MacroHost {
    func Test() {
        let message = "hello"
        let value = structuredBlocks! {
            started { message. }
            stopping { message. }
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new StructuredBlockMacro()));
        var firstPosition = code.IndexOf("message.", StringComparison.Ordinal) + "message.".Length;
        var secondPosition = code.LastIndexOf("message.", StringComparison.Ordinal) + "message.".Length;
        var completionService = new CompletionService();

        var firstItems = completionService.GetCompletions(compilation, syntaxTree, firstPosition);
        var secondItems = completionService.GetCompletions(compilation, syntaxTree, secondPosition);

        Assert.Contains(firstItems, static item => item.DisplayText == "Length");
        Assert.Contains(secondItems, static item => item.DisplayText == "Length");
    }

    [Theory]
    [InlineData(MacroFragmentKind.Statement, "mes", "message")]
    [InlineData(MacroFragmentKind.Type, "Wid", "Widget")]
    [InlineData(MacroFragmentKind.Pattern, "mes", "message")]
    [InlineData(MacroFragmentKind.MemberDeclaration, "class Nested(val item: Wid) { }", "Widget")]
    public void GetCompletions_InsideReportedFragment_UsesReportedSyntaxCategory(
        MacroFragmentKind kind,
        string fragment,
        string expectedCompletion)
    {
        var code = $$"""
class Widget { }

class MacroHost {
    func Test() {
        let message = "hello"
        let value = categorized!{ {{fragment}} }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new CategorizedFragmentMacro(kind)));

        var completionPrefix = kind == MacroFragmentKind.MemberDeclaration ? "Wid" : fragment;
        var position = code.LastIndexOf(completionPrefix, StringComparison.Ordinal) + completionPrefix.Length;
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        Assert.Contains(items, item => item.DisplayText == expectedCompletion);
    }

    [Fact]
    public void GetCompletions_InFreestandingMacroName_ReturnsOnlyFreestandingMacros()
    {
        const string code = """
class MacroHost {
    func Test() {
        let answer = que! { }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(
                new MacroReference(new ObservableMacro()),
                new MacroReference(new SubscribeMacro()),
                new MacroReference(new QueryMacro()));

        var position = code.IndexOf('!', code.IndexOf("que!", StringComparison.Ordinal));
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.DisplayText == "query");
        Assert.DoesNotContain(items, static item => item.DisplayText == "Observable");
    }

    [Fact]
    public void GetCompletions_InsideEmptyMacroAttribute_ReturnsOnlyAttachedMacros()
    {
        const string code = """
class CounterViewModel {
    #[]
    var Count: int = 0
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(
                new MacroReference(new ObservableMacro()),
                new MacroReference(new SubscribeMacro()),
                new MacroReference(new QueryMacro()));

        var position = code.IndexOf("#[", StringComparison.Ordinal) + 2;
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.DisplayText == "Observable");
        Assert.DoesNotContain(items, static item => item.DisplayText is "subscribe" or "query");
    }

    [Fact]
    public void GetCompletions_AfterHashInDeclaration_ReturnsOnlyAttachedMacros()
    {
        const string code = """
class CounterViewModel {
    #
    var Count: int = 0
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(
                new MacroReference(new ObservableMacro()),
                new MacroReference(new SubscribeMacro()),
                new MacroReference(new QueryMacro()));

        var position = code.IndexOf('#', StringComparison.Ordinal) + 1;
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var observable = Assert.Single(items.Where(static item => item.DisplayText == "Observable"));
        Assert.Equal("[Observable]", observable.InsertionText);
        Assert.DoesNotContain(items, static item => item.DisplayText is "subscribe" or "query");
    }

    [Fact]
    public void GetCompletions_InFreestandingMacroName_ReturnsLocalMacro()
    {
        const string code = """
class MacroHost {
    func Test() {
        let answer = local!()
    }
}
""";
        var macroTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Macros.*
            import Raven.Macros.*

            class LocalAnswerMacro : IMacroDefinition {
                val Name: string => "localAnswer"
                val Kind: MacroKind => MacroKind.Freestanding

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    FreestandingMacroExpansionResult {
                        Expression = quote!{ 42 }
                    }
                }
            }
            """);
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddMacroSyntaxTrees(macroTree)
            .AddSyntaxTrees(syntaxTree);

        var position = code.IndexOf('!', code.IndexOf("local!", StringComparison.Ordinal));
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        var localAnswer = Assert.Single(items.Where(static item => item.DisplayText == "localAnswer"));
        Assert.Equal("localAnswer", localAnswer.InsertionText);
    }

    [Fact]
    public void GetCompletions_InFreestandingMacroName_ReturnsIntrinsicQuote()
    {
        const string code = """
import Raven.Macros.*

class MacroHost {
    func Test() {
        let syntax = quo!()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.RavenMacros);

        var position = code.IndexOf('!', code.IndexOf("quo!", StringComparison.Ordinal));
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        var quote = Assert.Single(items.Where(static item => item.DisplayText == "quote"));
        Assert.Equal("quote", quote.InsertionText);
        Assert.Null(quote.CursorOffset);
        Assert.Contains("token-tree body", quote.Description, StringComparison.OrdinalIgnoreCase);
        var symbol = Assert.IsAssignableFrom<IMacroSymbol>(quote.Symbol);
        Assert.Equal(SymbolKind.Macro, symbol.Kind);
        Assert.Equal("Raven.Macros.Quote", symbol.CanonicalName);
    }

    [Fact]
    public void GetCompletions_WithoutMacroNamespaceImport_DoesNotReturnQuoteAlias()
    {
        const string code = """
class MacroHost {
    func Test() {
        let syntax = quo! { 42 }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.RavenMacros);

        var position = code.IndexOf('!', code.IndexOf("quo!", StringComparison.Ordinal));
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        Assert.DoesNotContain(items, static item => item.DisplayText == "quote");
    }

    [Fact]
    public void GetCompletions_InFreestandingMacroName_PreservesInvocationSuffix()
    {
        const string code = """
import Raven.Macros.*

class MacroHost {
    func Test() {
        let syntax = quo! { 42 }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.RavenMacros);

        var position = code.IndexOf('!', code.IndexOf("quo!", StringComparison.Ordinal));
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        var quote = Assert.Single(items.Where(static item => item.DisplayText == "quote"));
        Assert.Equal("quote", quote.InsertionText);
        Assert.Null(quote.CursorOffset);
    }

    [Fact]
    public void GetCompletions_InFreestandingMacroName_ReturnsIntrinsicCompile()
    {
        const string code = """
import Raven.Macros.*

class MacroHost {
    func Test() {
        let increment = comp! { value => value + 1 }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.RavenMacros);

        var position = code.IndexOf('!', code.IndexOf("comp!", StringComparison.Ordinal));
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        var compile = Assert.Single(items.Where(static item => item.DisplayText == "compile"));
        Assert.Equal("compile", compile.InsertionText);
        Assert.Null(compile.CursorOffset);
    }

    [Fact]
    public void GetCompletions_InMacroAttributeName_ReturnsAttachedMacros()
    {
        const string code = """
class CounterViewModel {
    #[Obs]
    var Count: int = 0
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(
                new MacroReference(new ObservableMacro()),
                new MacroReference(new SubscribeMacro()),
                new MacroReference(new QueryMacro()));

        var position = code.IndexOf(']', StringComparison.Ordinal);
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var observable = Assert.Single(items.Where(static item => item.DisplayText == "Observable"));
        Assert.Equal("Observable", observable.InsertionText);
        Assert.Contains("attached declaration macro", observable.Description, StringComparison.OrdinalIgnoreCase);
        Assert.Contains("Property", observable.Description, StringComparison.Ordinal);
    }

    [Fact]
    public void GetCompletions_InFreestandingMacroName_ReturnsFreestandingMacros()
    {
        const string code = """
class MacroHost {
    func Test() {
        let subscription = sub!()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(
                new MacroReference(new ObservableMacro()),
                new MacroReference(new SubscribeMacro()),
                new MacroReference(new QueryMacro()));

        var position = code.IndexOf('!', code.IndexOf("sub!", StringComparison.Ordinal));
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var token = syntaxTree.GetRoot().FindToken(Math.Max(0, position - 1));
        var directItems = CompletionProvider.GetCompletions(token, semanticModel, position).ToList();
        Assert.Contains(directItems, static item => item.DisplayText == "subscribe");

        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var subscribe = Assert.Single(items.Where(static item => item.DisplayText == "subscribe"));
        Assert.Equal("subscribe", subscribe.InsertionText);
        Assert.Null(subscribe.CursorOffset);
        Assert.Contains("freestanding procedural macro", subscribe.Description, StringComparison.OrdinalIgnoreCase);
        Assert.Contains("accepts arguments", subscribe.Description, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void GetCompletions_UsesDescriptorCapturedDuringMacroRegistration()
    {
        const string code = """
class MacroHost {
    func Test() {
        let value = capt!()
    }
}
""";

        var macro = new DescriptorSnapshotMacro();
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(macro));

        var position = code.IndexOf('!', code.IndexOf("capt!", StringComparison.Ordinal));
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.DisplayText == "captured");
        Assert.Equal(1, macro.AcceptsArgumentsReadCount);
    }

    [Fact]
    public void GetCompletions_InFreestandingMacroName_UsesTokenTreeInsertion()
    {
        const string code = """
class MacroHost {
    func Test() {
        let query = que!()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(
                new MacroReference(new ObservableMacro()),
                new MacroReference(new SubscribeMacro()),
                new MacroReference(new QueryMacro()));

        var position = code.IndexOf('!', code.IndexOf("que!", StringComparison.Ordinal));
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var query = Assert.Single(items.Where(static item => item.DisplayText == "query"));
        Assert.Equal("query", query.InsertionText);
        Assert.Null(query.CursorOffset);
        Assert.Contains("token-tree body", query.Description, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void GetCompletions_ForTypedTokenTreeMacro_InsertsArgumentsBeforeBody()
    {
        const string code = """
class MacroHost {
    func Test() {
        let query = typed!()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new TypedQueryMacro()));

        var position = code.IndexOf('!', code.IndexOf("typed!", StringComparison.Ordinal));
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var query = Assert.Single(items.Where(static item => item.DisplayText == "typedQuery"));
        Assert.Equal("typedQuery", query.InsertionText);
        Assert.Null(query.CursorOffset);
        Assert.Contains("arguments and a token-tree body", query.Description, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void GetCompletions_InTypedTokenTreeArguments_ReturnsNamedParameters()
    {
        const string code = """
class MacroHost {
    func Test() {
        let query = typedQuery!(Di) {
            query content
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new TypedQueryMacro()));

        var position = code.IndexOf("Di)", StringComparison.Ordinal) + 2;
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var dialect = Assert.Single(items.Where(static item => item.DisplayText == "Dialect"));
        Assert.Equal("Dialect: ", dialect.InsertionText);
        Assert.Equal("Di", code.Substring(dialect.ReplacementSpan.Start, dialect.ReplacementSpan.Length));
        Assert.Equal("macro argument: string", dialect.Description);
    }

    [Fact]
    public void GetCompletions_InTypedTokenTreeArguments_OmitsAlreadyNamedParameters()
    {
        const string code = """
class MacroHost {
    func Test() {
        let query = typedQuery!(Dialect: "sql", ) {
            query content
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new TypedQueryMacro()));

        var position = code.IndexOf(", )", StringComparison.Ordinal) + 2;
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.DoesNotContain(items, static item => item.DisplayText == "Dialect");
        var optimize = Assert.Single(items.Where(static item => item.DisplayText == "Optimize"));
        Assert.Equal("Optimize: ", optimize.InsertionText);
        Assert.Equal("macro argument: bool", optimize.Description);
    }

    [Fact]
    public void GetCompletions_InsideTypedMacroArgumentValue_DoesNotOfferParameterNames()
    {
        const string code = """
class MacroHost {
    func Test() {
        let query = typedQuery!(Dialect: "sql") {
            query content
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new TypedQueryMacro()));

        var position = code.IndexOf("sql", StringComparison.Ordinal) + 2;
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.DoesNotContain(items, static item => item.DisplayText is "Dialect" or "Optimize");
    }

    [Fact]
    public void GetCompletions_InTypedAttachedMacroArguments_ReturnsNamedParameters()
    {
        const string code = """
class ViewModel {
    #[TypedObservable(No)]
    var Title: string = ""
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new TypedObservableMacro()));

        var position = code.IndexOf("No)", StringComparison.Ordinal) + 2;
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var notify = Assert.Single(items.Where(static item => item.DisplayText == "Notify"));
        Assert.Equal("Notify: ", notify.InsertionText);
        Assert.Equal("macro argument: bool", notify.Description);
    }

    [Fact]
    public void GetCompletions_InTypedFreestandingMacroArguments_ReturnsNamedParameters()
    {
        const string code = """
class MacroHost {
    func Test() {
        let answer = typedCall!(Mo)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new TypedCallMacro()));

        var position = code.IndexOf("Mo)", StringComparison.Ordinal) + 2;
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var mode = Assert.Single(items.Where(static item => item.DisplayText == "Mode"));
        Assert.Equal("Mode: ", mode.InsertionText);
        Assert.Equal("macro argument: string", mode.Description);
    }

    private sealed class ObservableMacro : IMacroDefinition
    {
        public string Namespace => string.Empty;

        public string Name => "Observable";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => MacroExpansionResult.Empty;
    }

    private sealed class FragmentMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Namespace => string.Empty;

        public string Name => "fragment";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(0, context.BodySpan.Length)),
            ];
    }

    private sealed class EmptyFragmentMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Namespace => string.Empty;

        public string Name => "emptyFragment";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(context.BodySpan.Length, 0)),
            ];
    }

    private sealed class StructuredBlockMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Namespace => string.Empty;

        public string Name => "structuredBlocks";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
        {
            var body = context.GetBodyText();
            var firstStart = body.IndexOf("message.", StringComparison.Ordinal);
            var secondStart = body.LastIndexOf("message.", StringComparison.Ordinal);
            return
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Block,
                    new TextSpan(firstStart, "message.".Length)),
                context.CreateFragmentRegion(
                    MacroFragmentKind.Block,
                    new TextSpan(secondStart, "message.".Length)),
            ];
        }
    }

    private sealed class CompletionDslMacro : IMacroDefinition, IMacroCompletionProvider
    {
        public string Namespace => string.Empty;

        public string Name => "completionDsl";

        public bool SawCancellableToken { get; private set; }

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroCompletionItem> GetCompletions(
            TokenTreeMacroContext context,
            int bodyRelativePosition)
        {
            SawCancellableToken = context.CancellationToken.CanBeCanceled;
            var body = context.GetBodyText();
            var start = body.IndexOf("Wid", StringComparison.Ordinal);
            return
            [
                new MacroCompletionItem(
                    "Widget",
                    "Widget",
                    new TextSpan(start, bodyRelativePosition - start),
                    Description: "macro DSL item"),
            ];
        }
    }

    private sealed class OuterBlockMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Namespace => string.Empty;

        public string Name => "outerBlock";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Block,
                    new TextSpan(0, context.BodySpan.Length)),
            ];
    }

    private sealed class CategorizedFragmentMacro(MacroFragmentKind kind) :
        IMacroDefinition,
        IMacroFragmentProvider
    {
        public string Namespace => string.Empty;

        public string Name => "categorized";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    kind,
                    new TextSpan(0, context.BodySpan.Length)),
            ];
    }

    private sealed class SubscribeMacro : IMacroDefinition
    {
        public string Namespace => string.Empty;

        public string Name => "subscribe";

        public bool AcceptsArguments => true;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }

    private sealed class DescriptorSnapshotMacro : IMacroDefinition
    {
        public string Namespace => string.Empty;

        public string Name => "captured";

        public int AcceptsArgumentsReadCount { get; private set; }

        public bool AcceptsArguments
        {
            get
            {
                AcceptsArgumentsReadCount++;
                return true;
            }
        }

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }

    private sealed class QueryMacro : IMacroDefinition
    {
        public string Namespace => string.Empty;

        public string Name => "query";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }

    private sealed class TypedQueryParameters
    {
        public string Dialect { get; set; } = string.Empty;

        public bool Optimize { get; set; }
    }

    private sealed class TypedQueryMacro : IMacroDefinition
    {
        public string Namespace => string.Empty;

        public string Name => "typedQuery";

        public FreestandingMacroExpansionResult Expand(
            string Dialect,
            bool Optimize,
            TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }

    private sealed class TypedObservableParameters
    {
        public bool Notify { get; set; }
    }

    private sealed class TypedObservableMacro : IMacroDefinition
    {
        public string Namespace => string.Empty;

        public string Name => "TypedObservable";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(bool Notify, AttachedMacroContext context)
            => MacroExpansionResult.Empty;
    }

    private sealed class TypedCallParameters
    {
        public string Mode { get; set; } = string.Empty;
    }

    private sealed class TypedCallMacro : IMacroDefinition
    {
        public string Namespace => string.Empty;

        public string Name => "typedCall";

        public FreestandingMacroExpansionResult Expand(string Mode, FreestandingMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }
}
