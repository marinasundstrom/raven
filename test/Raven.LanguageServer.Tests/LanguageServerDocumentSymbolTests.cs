using System.Reflection;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

namespace Raven.LanguageServer.Tests;

public sealed class LanguageServerDocumentSymbolTests
{
    [Fact]
    public async Task Handle_OpenDocument_UsesSyntaxOnlyContextAsync()
    {
        var tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-document-symbols-{Guid.NewGuid():N}");
        try
        {
            Directory.CreateDirectory(tempRoot);
            var workspace = Raven.CodeAnalysis.RavenWorkspace.Create(targetFramework: "net10.0");
            var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
            manager.Initialize(new InitializeParams
            {
                WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
                {
                    Name = "temp",
                    Uri = DocumentUri.FromFileSystemPath(tempRoot)
                })
            });

            var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
            var handler = new DocumentSymbolHandler(store, NullLogger<DocumentSymbolHandler>.Instance);
            var uri = DocumentUri.FromFileSystemPath(Path.Combine(tempRoot, "main.rvn"));
            await store.UpsertDocumentAsync(uri, """
let builder = WebApplication.CreateBuilder(args)
let app = builder.Build()

app.MapGet("/", func () => "Hello")

record PingResult(val Message: string)

component! Greeting(Name: string) {
    markup! { <h1>Hello {Name}</h1> }
}
""");

            var result = await handler.Handle(
                new DocumentSymbolParams { TextDocument = new TextDocumentIdentifier(uri) },
                CancellationToken.None);

            var symbols = result!.ToArray();
            symbols.Select(static symbol => symbol.DocumentSymbol!.Name).ShouldContain("<top-level code>");
            symbols.Select(static symbol => symbol.DocumentSymbol!.Name).ShouldContain("PingResult");
            var greeting = symbols
                .Select(static symbol => symbol.DocumentSymbol!)
                .Single(symbol => symbol.Name == "Greeting");
            greeting.Kind.ShouldBe(SymbolKind.Object);
            greeting.Detail.ShouldBe("component!");
        }
        finally
        {
            if (Directory.Exists(tempRoot))
                Directory.Delete(tempRoot, recursive: true);
        }
    }

    [Fact]
    public void Outline_IncludesVariablesAndSyntheticTopLevelCodeForExecutableGlobalStatements()
    {
        const string code = """
let port = 8080
print(port)
""";

        var symbols = GetDocumentSymbols(code);

        symbols.Count.ShouldBe(2);
        symbols.Single(symbol => symbol.Name == "port").Kind.ShouldBe(SymbolKind.Variable);
        var topLevelCode = symbols.Single(symbol => symbol.Name == "<top-level code>");
        topLevelCode.Kind.ShouldBe(SymbolKind.Function);
        topLevelCode.Children.ShouldBeNull();
    }

    [Fact]
    public void Outline_UsesSingleTopLevelCodeSymbol_WhenStatementsAreInterleavedWithDeclarations()
    {
        const string code = """
let a = 1

func ping() -> int => a

let b = 2

func ping1() -> int => b

print(a + b)

class Data {
    val Value: int
}
""";

        var symbols = GetDocumentSymbols(code);

        symbols.Count(symbol => symbol.Name == "<top-level code>").ShouldBe(1);
        symbols.Single(symbol => symbol.Name == "ping").Kind.ShouldBe(SymbolKind.Function);
        symbols.Single(symbol => symbol.Name == "ping1").Kind.ShouldBe(SymbolKind.Function);
        symbols.Single(symbol => symbol.Name == "Data").Kind.ShouldBe(SymbolKind.Class);
    }

    [Fact]
    public void Outline_NestsLocalFunctions_UnderContainingCallableDeclarations()
    {
        const string code = """
func Main() -> int {
    func Parse() -> int {
        func Leaf() -> int => 42
        Leaf()
    }

    Parse()
}

class C {
    func Run() -> int {
        func Inner() -> int => 1
        Inner()
    }
}
""";

        var symbols = GetDocumentSymbols(code);

        var main = symbols.Single(symbol => symbol.Name == "Main");
        main.Kind.ShouldBe(SymbolKind.Function);
        main.Children.ShouldNotBeNull();
        main.Children.Count().ShouldBe(1);
        var parse = main.Children.Single();
        parse.Name.ShouldBe("Parse");
        parse.Kind.ShouldBe(SymbolKind.Function);
        parse.Children.ShouldNotBeNull();
        parse.Children.Count().ShouldBe(1);
        parse.Children.Single().Name.ShouldBe("Leaf");

        var type = symbols.Single(symbol => symbol.Name == "C");
        type.Kind.ShouldBe(SymbolKind.Class);
        type.Children.ShouldNotBeNull();
        var run = type.Children.Single(symbol => symbol.Name == "Run");
        run.Kind.ShouldBe(SymbolKind.Method);
        run.Children.ShouldNotBeNull();
        run.Children.Count().ShouldBe(1);
        var inner = run.Children.Single();
        inner.Name.ShouldBe("Inner");
        inner.Kind.ShouldBe(SymbolKind.Function);
    }

    [Fact]
    public void Outline_TopLevelCodeSymbol_ContainsNestedLocalFunctions()
    {
        const string code = """
if true {
    func Bootstrap() -> int => 42
    Bootstrap()
}
""";

        var symbols = GetDocumentSymbols(code);

        symbols.Count.ShouldBe(1);
        symbols[0].Name.ShouldBe("<top-level code>");
        symbols[0].Children.ShouldNotBeNull();
        symbols[0].Children.Count().ShouldBe(1);
        var bootstrap = symbols[0].Children.Single();
        bootstrap.Name.ShouldBe("Bootstrap");
        bootstrap.Kind.ShouldBe(SymbolKind.Function);
    }

    [Fact]
    public void Outline_IncludesMacros_WithDistinctSymbolKind()
    {
        const string code = """
namespace Tools {
    macro Quote(body: ExpressionSyntax) -> ExpressionSyntax {
        func Preserve() -> ExpressionSyntax => body
        return Preserve()
    }
}
""";

        var symbols = GetDocumentSymbols(code);

        var tools = symbols.Single();
        tools.Name.ShouldBe("Tools");
        tools.Kind.ShouldBe(SymbolKind.Namespace);
        tools.Children.ShouldNotBeNull();

        var quote = tools.Children.Single();
        quote.Name.ShouldBe("Quote");
        quote.Kind.ShouldBe(SymbolKind.Operator);
        quote.Children.ShouldNotBeNull();
        var preserve = quote.Children.Single();
        preserve.Name.ShouldBe("Preserve");
        preserve.Kind.ShouldBe(SymbolKind.Function);
    }

    [Fact]
    public void Outline_IncludesIdentifierBearingDeclarationMacroCarriers()
    {
        const string code = """
component! Greeting(Name: string) {
    markup! { <h1>Hello {Name}</h1> }
}

namespace Actors {
    public actor! ShoppingCart(Id: string) {
        receive AddItem
    }
}

class Dashboard {
    component! Header(Title: string) {
        markup! { <h1>{Title}</h1> }
    }

    GenerateMembers!() { Id, Title }
}
""";

        var symbols = GetDocumentSymbols(code);

        var greeting = symbols.Single(symbol => symbol.Name == "Greeting");
        greeting.Kind.ShouldBe(SymbolKind.Object);
        greeting.Detail.ShouldBe("component!");

        var actors = symbols.Single(symbol => symbol.Name == "Actors");
        var shoppingCart = actors.Children.ShouldHaveSingleItem();
        shoppingCart.Name.ShouldBe("ShoppingCart");
        shoppingCart.Kind.ShouldBe(SymbolKind.Object);
        shoppingCart.Detail.ShouldBe("actor!");

        var dashboard = symbols.Single(symbol => symbol.Name == "Dashboard");
        dashboard.Children.ShouldNotBeNull();
        var header = dashboard.Children.ShouldHaveSingleItem();
        header.Name.ShouldBe("Header");
        header.Kind.ShouldBe(SymbolKind.Object);
        header.Detail.ShouldBe("component!");
    }

    [Fact]
    public void Outline_IncludesNamespaceScopedConstantsAndVariables()
    {
        const string code = """
namespace Hardware {
    extern const LedPin: int = 25
    const DefaultPin: int = 5
    let currentPin = 7
    var mutablePin = 8

    func Read() -> int => currentPin
}
""";

        var hardware = GetDocumentSymbols(code).ShouldHaveSingleItem();
        hardware.Children.ShouldNotBeNull();
        var children = hardware.Children.ToArray();

        children.Single(symbol => symbol.Name == "LedPin").Kind.ShouldBe(SymbolKind.Constant);
        children.Single(symbol => symbol.Name == "DefaultPin").Kind.ShouldBe(SymbolKind.Constant);
        children.Single(symbol => symbol.Name == "currentPin").Kind.ShouldBe(SymbolKind.Variable);
        children.Single(symbol => symbol.Name == "mutablePin").Kind.ShouldBe(SymbolKind.Variable);
        children.Single(symbol => symbol.Name == "Read").Kind.ShouldBe(SymbolKind.Function);
        children.ShouldNotContain(symbol => symbol.Name == "<top-level code>");
    }

    private static IReadOnlyList<DocumentSymbol> GetDocumentSymbols(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rvn");
        var text = syntaxTree.GetText();
        var root = syntaxTree.GetRoot();
        var buildMemberSymbols = typeof(DocumentSymbolHandler)
            .GetMethod("BuildMemberSymbols", BindingFlags.NonPublic | BindingFlags.Static)!;

        return ((IEnumerable<DocumentSymbol>)buildMemberSymbols.Invoke(null, [root.Members, text])!)
            .ToArray();
    }
}
