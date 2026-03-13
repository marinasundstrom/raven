using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public class LanguageServerDefinitionMappingTests
{
    [Fact]
    public void BuildLocationLinks_ForMethodSymbol_ProvidesOriginAndTargetRanges()
    {
        const string code = """
class Counter {
    func Increment() -> unit { }
    func Run() -> unit {
        self.Increment()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in LanguageServerTestReferences.Default)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var invocation = root.DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var methodSymbol = semanticModel.GetSymbolInfo(invocation).Symbol.ShouldBeAssignableTo<IMethodSymbol>();

        var links = DefinitionLocationMapper
            .BuildLocationLinks(methodSymbol, syntaxTree.GetText(), invocation.Expression.Span)
            .ToArray();

        links.Length.ShouldBeGreaterThan(0);

        var first = links[0];
        first.TargetUri.ShouldBe(DocumentUri.FromFileSystemPath("/workspace/test.rav"));
        first.OriginSelectionRange.Start.Line.ShouldBe(3);
        first.TargetSelectionRange.Start.Line.ShouldBe(1);
    }
}
