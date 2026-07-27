using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public sealed class CompletionServiceMacroTests
{
    [Fact]
    public void GetCompletions_AfterHashInExpression_ReturnsOnlyFreestandingMacros()
    {
        const string code = """
class MacroHost {
    func Test() {
        val answer = #
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

        var position = code.IndexOf('#', StringComparison.Ordinal) + 1;
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, static item => item.DisplayText == "subscribe");
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
        val answer = #local()
    }
}
""";
        var macroTree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Macros.*

            class LocalAnswerMacro : ITokenTreeExpressionMacro {
                val Name: string => "localAnswer"
                val Kind: MacroKind => MacroKind.FreestandingExpression

                func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                    FreestandingMacroExpansionResult {
                        Expression = #quote { 42 }
                    }
                }
            }
            """);
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroSyntaxTrees(macroTree)
            .AddSyntaxTrees(syntaxTree);

        var position = code.IndexOf('(', code.IndexOf("#local", StringComparison.Ordinal));
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        var localAnswer = Assert.Single(items.Where(static item => item.DisplayText == "localAnswer"));
        Assert.Equal("localAnswer { }", localAnswer.InsertionText);
    }

    [Fact]
    public void GetCompletions_InFreestandingMacroName_ReturnsIntrinsicQuote()
    {
        const string code = """
class MacroHost {
    func Test() {
        val syntax = #quo()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        var position = code.IndexOf('(', code.IndexOf("#quo", StringComparison.Ordinal));
        var items = new CompletionService()
            .GetCompletions(compilation, syntaxTree, position)
            .ToList();

        var quote = Assert.Single(items.Where(static item => item.DisplayText == "quote"));
        Assert.Equal("quote { }", quote.InsertionText);
        Assert.Equal(quote.InsertionText.Length - 1, quote.CursorOffset);
        Assert.Contains("token-tree body", quote.Description, StringComparison.OrdinalIgnoreCase);
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
        val subscription = #sub()
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

        var position = code.IndexOf('(', code.IndexOf("#sub", StringComparison.Ordinal));
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var token = syntaxTree.GetRoot().FindToken(Math.Max(0, position - 1));
        var directItems = CompletionProvider.GetCompletions(token, semanticModel, position).ToList();
        Assert.Contains(directItems, static item => item.DisplayText == "subscribe");

        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var subscribe = Assert.Single(items.Where(static item => item.DisplayText == "subscribe"));
        Assert.Equal("subscribe()", subscribe.InsertionText);
        Assert.Equal(subscribe.InsertionText.Length - 1, subscribe.CursorOffset);
        Assert.Contains("freestanding expression macro", subscribe.Description, StringComparison.OrdinalIgnoreCase);
        Assert.Contains("accepts arguments", subscribe.Description, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void GetCompletions_InFreestandingMacroName_UsesTokenTreeInsertion()
    {
        const string code = """
class MacroHost {
    func Test() {
        val query = #que()
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

        var position = code.IndexOf('(', code.IndexOf("#que", StringComparison.Ordinal));
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var query = Assert.Single(items.Where(static item => item.DisplayText == "query"));
        Assert.Equal("query { }", query.InsertionText);
        Assert.Equal(query.InsertionText.Length - 1, query.CursorOffset);
        Assert.Contains("token-tree body", query.Description, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void GetCompletions_ForTypedTokenTreeMacro_InsertsArgumentsBeforeBody()
    {
        const string code = """
class MacroHost {
    func Test() {
        val query = #typed()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new TypedQueryMacro()));

        var position = code.IndexOf('(', code.IndexOf("#typed", StringComparison.Ordinal));
        var items = new CompletionService().GetCompletions(compilation, syntaxTree, position).ToList();

        var query = Assert.Single(items.Where(static item => item.DisplayText == "typedQuery"));
        Assert.Equal("typedQuery() { }", query.InsertionText);
        Assert.Equal("typedQuery".Length + 1, query.CursorOffset);
        Assert.Contains("arguments and a token-tree body", query.Description, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void GetCompletions_InTypedTokenTreeArguments_ReturnsNamedParameters()
    {
        const string code = """
class MacroHost {
    func Test() {
        val query = #typedQuery(Di) {
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
        val query = #typedQuery(Dialect: "sql", ) {
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
        val query = #typedQuery(Dialect: "sql") {
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
        val answer = #typedCall(Mo)
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

    private sealed class ObservableMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => MacroExpansionResult.Empty;
    }

    private sealed class SubscribeMacro : IFreestandingExpressionMacro
    {
        public string Name => "subscribe";

        public bool AcceptsArguments => true;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }

    private sealed class QueryMacro : ITokenTreeExpressionMacro
    {
        public string Name => "query";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }

    private sealed class TypedQueryParameters
    {
        public string Dialect { get; set; } = string.Empty;

        public bool Optimize { get; set; }
    }

    private sealed class TypedQueryMacro : ITokenTreeExpressionMacro<TypedQueryParameters>
    {
        public string Name => "typedQuery";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext<TypedQueryParameters> context)
            => FreestandingMacroExpansionResult.Empty;
    }

    private sealed class TypedObservableParameters
    {
        public bool Notify { get; set; }
    }

    private sealed class TypedObservableMacro : IAttachedDeclarationMacro<TypedObservableParameters>
    {
        public string Name => "TypedObservable";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext<TypedObservableParameters> context)
            => MacroExpansionResult.Empty;
    }

    private sealed class TypedCallParameters
    {
        public string Mode { get; set; } = string.Empty;
    }

    private sealed class TypedCallMacro : IFreestandingExpressionMacro<TypedCallParameters>
    {
        public string Name => "typedCall";

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext<TypedCallParameters> context)
            => FreestandingMacroExpansionResult.Empty;
    }
}
