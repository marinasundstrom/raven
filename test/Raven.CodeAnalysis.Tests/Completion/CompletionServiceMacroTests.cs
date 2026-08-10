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
    public void GetCompletions_InInvocableMacroName_ReturnsOnlyInvocableMacros()
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
    public void GetCompletions_InInvocableMacroName_ReturnsLocalMacro()
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

            class LocalAnswerMacro : ITokenTreeMacro {
                val Name: string => "localAnswer"
                val Kind: MacroKind => MacroKind.Invocable

                func Expand(context: TokenTreeMacroContext) -> InvocableMacroExpansionResult {
                    InvocableMacroExpansionResult {
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
    public void GetCompletions_InInvocableMacroName_ReturnsIntrinsicQuote()
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
    public void GetCompletions_InInvocableMacroName_PreservesInvocationSuffix()
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
    public void GetCompletions_InInvocableMacroName_ReturnsIntrinsicCompile()
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
    public void GetCompletions_InInvocableMacroName_ReturnsInvocableMacros()
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
        Assert.Contains("invocable macro", subscribe.Description, StringComparison.OrdinalIgnoreCase);
        Assert.Contains("accepts arguments", subscribe.Description, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void GetCompletions_InInvocableMacroName_UsesTokenTreeInsertion()
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
    public void GetCompletions_InTypedInvocableMacroArguments_ReturnsNamedParameters()
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

    private sealed class ObservableMacro : IAttachedDeclarationMacro
    {
        public string Namespace => string.Empty;

        public string Name => "Observable";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
            => MacroExpansionResult.Empty;
    }

    private sealed class FragmentMacro : ITokenTreeMacro, IMacroFragmentProvider
    {
        public string Namespace => string.Empty;

        public string Name => "fragment";

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(0, context.BodySpan.Length)),
            ];
    }

    private sealed class EmptyFragmentMacro : ITokenTreeMacro, IMacroFragmentProvider
    {
        public string Namespace => string.Empty;

        public string Name => "emptyFragment";

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(context.BodySpan.Length, 0)),
            ];
    }

    private sealed class CategorizedFragmentMacro(MacroFragmentKind kind) :
        ITokenTreeMacro,
        IMacroFragmentProvider
    {
        public string Namespace => string.Empty;

        public string Name => "categorized";

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    kind,
                    new TextSpan(0, context.BodySpan.Length)),
            ];
    }

    private sealed class SubscribeMacro : IInvocableMacro
    {
        public string Namespace => string.Empty;

        public string Name => "subscribe";

        public bool AcceptsArguments => true;

        public InvocableMacroExpansionResult Expand(InvocableMacroContext context)
            => InvocableMacroExpansionResult.Empty;
    }

    private sealed class QueryMacro : ITokenTreeMacro
    {
        public string Namespace => string.Empty;

        public string Name => "query";

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.Empty;
    }

    private sealed class TypedQueryParameters
    {
        public string Dialect { get; set; } = string.Empty;

        public bool Optimize { get; set; }
    }

    private sealed class TypedQueryMacro : ITokenTreeMacro<TypedQueryParameters>
    {
        public string Namespace => string.Empty;

        public string Name => "typedQuery";

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext<TypedQueryParameters> context)
            => InvocableMacroExpansionResult.Empty;
    }

    private sealed class TypedObservableParameters
    {
        public bool Notify { get; set; }
    }

    private sealed class TypedObservableMacro : IAttachedDeclarationMacro<TypedObservableParameters>
    {
        public string Namespace => string.Empty;

        public string Name => "TypedObservable";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext<TypedObservableParameters> context)
            => MacroExpansionResult.Empty;
    }

    private sealed class TypedCallParameters
    {
        public string Mode { get; set; } = string.Empty;
    }

    private sealed class TypedCallMacro : IInvocableMacro<TypedCallParameters>
    {
        public string Namespace => string.Empty;

        public string Name => "typedCall";

        public InvocableMacroExpansionResult Expand(InvocableMacroContext<TypedCallParameters> context)
            => InvocableMacroExpansionResult.Empty;
    }
}
