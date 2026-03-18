using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public sealed class CompletionServiceMacroTests
{
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
            .AddMacroReferences(new MacroReference(typeof(ReactiveMacroPlugin)));

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
            .AddMacroReferences(new MacroReference(typeof(ReactiveMacroPlugin)));

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

    public sealed class ReactiveMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(ReactiveMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ObservableMacro(), new SubscribeMacro()];
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

        public MacroTarget Targets => MacroTarget.None;

        public bool AcceptsArguments => true;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
            => FreestandingMacroExpansionResult.Empty;
    }
}
