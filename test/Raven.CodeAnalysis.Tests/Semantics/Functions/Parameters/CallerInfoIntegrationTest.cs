using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;
namespace Raven.CodeAnalysis.Tests;

public sealed class CallerInfoIntegrationTest
{
    [Fact, Trait("Target", "DotNet")]
    public void QueryingFieldInitializerRetainsConstructorParameterAndCallerInformation()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Runtime.CompilerServices.*
            class Holder {
                val Captured: string
                init(value: int, [CallerArgumentExpression("value")] expression: string = "fallback", [CallerMemberName] member: string = "fallback") {
                    Captured = "${member}|${expression}"
                }
            }
            public class Probe {
                private static field saved: Holder = Holder(20 + 22)
                static func Run() -> string => saved.Captured
            }
            """);
        var compilation = Compilation.Create("CallerIntegration", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var constructor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        var syntax = constructor.ParameterList.Parameters[1];
        var parameter = Assert.IsAssignableFrom<ParameterSymbol>(model.GetDeclaredSymbol(syntax));
        Assert.Equal("fallback", parameter.ExplicitDefaultValue);
        Assert.Single(parameter.GetAttributes());
        var creation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<IObjectCreationOperation>(model.GetOperation(creation));
        Assert.Same(parameter, operation.Constructor!.Parameters[1]);
        Assert.Empty(compilation.GetDiagnostics().Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));
        Assert.Same(parameter, model.GetDeclaredSymbol(syntax));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal("saved|20 + 22", loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
