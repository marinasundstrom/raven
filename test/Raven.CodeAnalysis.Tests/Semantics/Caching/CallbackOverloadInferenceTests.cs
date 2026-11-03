using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class CallbackOverloadInferenceTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void GenericCallbackResultIsPreservedAcrossQueryOrder(bool bindFirst, bool queryAvailableFirst)
    {
        const string source = """
            class Box<T> { }
            static class Runner {
                static func Run(callback: () -> unit) -> Box<unit> => Box<unit>()
                static func Run<T>(callback: () -> T) -> Box<T> => Box<T>()
                static func Run<T>(callback: () -> Box<T>) -> Box<T> => Box<T>()
            }
            func Main() -> unit {
                let operation = Runner.Run(() => 41)
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("CallbackInference", [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var initializer = declaration.Initializer!.Value;
        if (bindFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        if (queryAvailableFirst && model.TryGetAvailableTypeInfo(initializer, out var available))
            AssertIntResult(available.Type!);
        var local = Assert.IsAssignableFrom<LocalSymbol>(model.GetDeclaredSymbol(declaration));
        AssertIntResult(local.Type);
        AssertIntResult(model.GetTypeInfo(initializer).Type!);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    private static void AssertIntResult(TypeSymbol type)
    {
        var named = Assert.IsAssignableFrom<NamedTypeSymbol>(type);
        Assert.Equal("Box", named.Name);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(named.TypeArguments).SpecialType);
    }
}
