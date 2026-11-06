using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class UnitCallbackTests
{
    [Theory, Trait("Target", "DotNet")]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void DotNet_CompletionCallbackUsesConfiguredDelegateShape(bool genericFirst, bool legacyInhabitedUnit)
        => VerifySelection(genericFirst, TargetPlatform.DotNet, legacyInhabitedUnit);

    [Theory, Trait("Target", "NeoCLR")]
    [InlineData(false)]
    [InlineData(true)]
    public void NeoClr_CompletionCallbackSelectsInhabitedResult(bool genericFirst)
        => VerifySelection(genericFirst, TargetPlatform.NeoCLR, inhabitedUnit: true);

    private static void VerifySelection(bool genericFirst, TargetPlatform target, bool inhabitedUnit)
    {
        const string completion = "static func Pick(action: System.Action) -> int => 1";
        const string generic = "static func Pick<T>(callback: System.Func<T>) -> int => 2";
        var tree = SyntaxTree.ParseText($$"""
            class Example {
                {{(genericFirst ? generic : completion)}}
                {{(genericFirst ? completion : generic)}}
                static func Run() -> int => Pick(() => { })
            }
            """);
        var options = CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary);
        var references = TestMetadataReferences.Default.ToArray();
        if (inhabitedUnit)
        {
            var corePath = references.OfType<PortableExecutableReference>()
                .Single(reference => Path.GetFileName(reference.FilePath) == "System.Runtime.dll").FilePath!;
            using var core = Mono.Cecil.AssemblyDefinition.ReadAssembly(corePath);
            core.Name.Name = "NeoCLR.CoreProbe";
            core.Name.PublicKey = [];
            using var image = new MemoryStream();
            core.Write(image);
            references = [.. references, MetadataReference.CreateFromImage(image.ToArray())];
            options = CompilationOptions.NeoCLR.WithTargetPlatform(target)
                .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null);
        }
        var compilation = Compilation.Create("CallbackShape", [tree], references, options);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var selected = Assert.IsAssignableFrom<MethodSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol);
        Assert.Equal(inhabitedUnit ? "Func" : "Action", selected.Parameters.Single().Type.Name);
    }
}
