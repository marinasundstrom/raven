using System.Reflection;

using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public sealed class LanguageServerDocumentSymbolTests
{
    [Fact]
    public void Outline_IncludesSyntheticTopLevelCodeSymbol_ForExecutableGlobalStatements()
    {
        const string code = """
val port = 8080
print(port)
""";

        var symbols = GetDocumentSymbols(code);

        symbols.Count.ShouldBe(1);
        symbols[0].Name.ShouldBe("<top-level code>");
        symbols[0].Kind.ShouldBe(SymbolKind.Function);
        symbols[0].Children.ShouldBeNull();
    }

    [Fact]
    public void Outline_UsesSingleTopLevelCodeSymbol_WhenStatementsAreInterleavedWithDeclarations()
    {
        const string code = """
val a = 1

func ping() -> int => a

val b = 2

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
