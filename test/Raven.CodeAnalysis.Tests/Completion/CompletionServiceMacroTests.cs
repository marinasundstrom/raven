using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public sealed class CompletionServiceMacroTests
{
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
}
