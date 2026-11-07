using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class PartialSealedHierarchyTests
{
    public static IEnumerable<object[]> Cases()
    {
        foreach (var kind in new[] { "class", "record class", "interface" })
            foreach (var order in new[] { new[] { 0, 1, 2 }, new[] { 0, 2, 1 }, new[] { 1, 0, 2 }, new[] { 1, 2, 0 }, new[] { 2, 0, 1 }, new[] { 2, 1, 0 } })
                foreach (var diagnosticsFirst in new[] { false, true })
                    yield return new object[] { kind, order, diagnosticsFirst };
    }

    [Theory, MemberData(nameof(Cases)), Trait("Target", "DotNet")]
    public void DotNet_PartialPermitsSurvivesDeclarationOrder(string kind, int[] order, bool diagnosticsFirst)
        => Verify(kind, order, diagnosticsFirst, TargetPlatform.DotNet);

    [Theory, MemberData(nameof(Cases)), Trait("Target", "NeoCLR")]
    public void NeoClr_PartialPermitsSurvivesDeclarationOrder(string kind, int[] order, bool diagnosticsFirst)
        => Verify(kind, order, diagnosticsFirst, TargetPlatform.NeoCLR);

    private static void Verify(string kind, int[] order, bool diagnosticsFirst, TargetPlatform target)
    {
        var leafKind = kind == "record class" ? "record class" : "class";
        var trees = new[] {
            SyntaxTree.ParseText($"namespace Example\ninternal sealed partial {kind} Root {{ }}", path: "root.rvn"),
            SyntaxTree.ParseText($"namespace Example\ninternal {leafKind} Leaf : Root {{ }}", path: "leaf.rvn"),
            SyntaxTree.ParseText($"namespace Example\ninternal sealed partial {kind} Root permits Leaf {{ }}", path: "generated.rvn")
        };
        var refs = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
            .Select(path => MetadataReference.CreateFromFile(path)).ToArray();
        var options = CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary);
        if (target == TargetPlatform.NeoCLR)
        {
            var corePath = refs.OfType<PortableExecutableReference>()
                .Single(reference => Path.GetFileName(reference.FilePath) == "System.Runtime.dll").FilePath!;
            using var core = Mono.Cecil.AssemblyDefinition.ReadAssembly(corePath);
            core.Name.Name = "NeoCLR.CoreProbe";
            core.Name.PublicKey = [];
            using var image = new MemoryStream();
            core.Write(image);
            refs = [.. refs, MetadataReference.CreateFromImage(image.ToArray())];
            options = CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeTypeOfContract(null);
        }
        var compilation = Compilation.Create("PartialClosure", order.Select(i => trees[i]).ToArray(), refs, options);
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var rootDeclaration = trees[0].GetRoot().DescendantNodes().OfType<TypeDeclarationSyntax>().Single();
        var generatedDeclaration = trees[2].GetRoot().DescendantNodes().OfType<TypeDeclarationSyntax>().Single();
        var root = Assert.IsAssignableFrom<NamedTypeSymbol>(compilation.GetSemanticModel(trees[0]).GetDeclaredSymbol(rootDeclaration));
        var generated = compilation.GetSemanticModel(trees[2]).GetDeclaredSymbol(generatedDeclaration);
        Assert.Same(root, generated);
        Assert.True(root.IsSealedHierarchy);
        Assert.Equal("Leaf", Assert.Single(root.PermittedDirectSubtypes).Name);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
