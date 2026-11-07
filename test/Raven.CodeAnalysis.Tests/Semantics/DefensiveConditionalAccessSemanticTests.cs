using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class DefensiveConditionalAccessSemanticTests
{
    [Theory, InlineData(false), InlineData(true), Trait("Target", "DotNet")]
    public void DotNet_DefensiveAccessPreservesNullableResult(bool diagnosticsFirst)
        => Verify(diagnosticsFirst, TargetPlatform.DotNet);

    [Theory, InlineData(false), InlineData(true), Trait("Target", "NeoCLR")]
    public void NeoClrCli_DefensiveAccessPreservesNullableResult(bool diagnosticsFirst)
        => Verify(diagnosticsFirst, TargetPlatform.NeoCLR);

    private static void Verify(bool diagnosticsFirst, TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText("""
class Probe {
    static func Member(value: string) -> string? => value?.Trim()

    static func Invoke(value: System.Func<string>) -> string? => value?()

    static func Index(value: string[]) -> string? => value?[0]

    static func Literal() -> string? => "abc"?.Trim()
}
""");
        var references = TestMetadataReferences.Default;
        var options = CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary);
        if (target == TargetPlatform.NeoCLR)
        {
            var corePath = references.OfType<PortableExecutableReference>()
                .Single(reference => Path.GetFileName(reference.FilePath) == "System.Runtime.dll").FilePath!;
            using var core = Mono.Cecil.AssemblyDefinition.ReadAssembly(corePath);
            core.Name.Name = "NeoCLR.CoreProbe";
            core.Name.PublicKey = [];
            using var image = new MemoryStream();
            core.Write(image);
            references = [.. references, MetadataReference.CreateFromImage(image.ToArray())];
            options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeTypeOfContract(null);
        }
        var compilation = Compilation.Create("DefensiveAccess", [tree], references, options);
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var accesses = tree.GetRoot().DescendantNodes().OfType<ConditionalAccessExpressionSyntax>().ToArray();
        Assert.Equal(4, accesses.Length);
        foreach (var access in accesses)
        {
            Assert.True(model.GetTypeInfo(access.Expression).Type!.IsReferenceType);
            Assert.False(model.GetTypeInfo(access.Expression).Type!.IsNullable);
            Assert.True(model.GetTypeInfo(access).Type!.IsNullable);
        }
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
