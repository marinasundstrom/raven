using Raven.CodeAnalysis.Syntax;

using CS = Microsoft.CodeAnalysis.CSharp;
using CSApi = Microsoft.CodeAnalysis;
namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class TupleRoundTripTests
{
    [Fact]
    public void CSharpIndexerUseSeesTupleElementNames()
    {
        var tree = SyntaxTree.ParseText("public class Relay { val self[index: int]: (row: int, column: int) { get => (row: index, column: 1) } }");
        var refs = TestMetadataReferences.Default;
        var raven = Compilation.Create("Indexers", [tree], refs, CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream(); var emit = raven.Emit(image);
        Assert.True(emit.Success, string.Join("\n", emit.Diagnostics));
        var cs = CS.CSharpCompilation.Create("IndexerReader", [CS.CSharpSyntaxTree.ParseText("class Reader { static int Read(Relay r) => r[5].row; }")],
            refs.OfType<PortableExecutableReference>().Select(r => CSApi.MetadataReference.CreateFromFile(r.FilePath!)).Append(CSApi.MetadataReference.CreateFromImage(image.ToArray())),
            new CS.CSharpCompilationOptions(CSApi.OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(cs.GetDiagnostics().Where(d => d.Severity == CSApi.DiagnosticSeverity.Error));
    }

    [Fact]
    public void CSharpSubscriberSeesTupleElementNames()
    {
        var tree = SyntaxTree.ParseText("public class Relay { public static event Updated: System.Action<(row: int, column: int)> }");
        var refs = TestMetadataReferences.Default;
        var raven = Compilation.Create("Events", [tree], refs, CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream(); var emit = raven.Emit(image);
        Assert.True(emit.Success, string.Join("\n", emit.Diagnostics));
        var cs = CS.CSharpCompilation.Create("Subscriber", [CS.CSharpSyntaxTree.ParseText("class Subscriber { static void Hook() { Relay.Updated += value => System.Console.WriteLine(value.column); } }")],
            refs.OfType<PortableExecutableReference>().Select(r => CSApi.MetadataReference.CreateFromFile(r.FilePath!)).Append(CSApi.MetadataReference.CreateFromImage(image.ToArray())),
            new CS.CSharpCompilationOptions(CSApi.OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(cs.GetDiagnostics().Where(d => d.Severity == CSApi.DiagnosticSeverity.Error));
    }

    [Fact, Trait("Target", "DotNet")]
    public void DotNet_WideTupleExposesLogicalFields() => CheckTarget(TargetPlatform.DotNet);

    [Fact, Trait("Target", "NeoCLR")]
    public void NeoCLR_WideTupleExposesLogicalFields() => CheckTarget(TargetPlatform.NeoCLR);

    private static void CheckTarget(TargetPlatform target)
    {
        var names = Enumerable.Range(1, 15).Select(i => $"v{i}").ToArray();
        var signature = string.Join(", ", names.Select(n => $"{n}: int"));
        var tree = SyntaxTree.ParseText($"class SourceTuple {{ static func Last(value: ({signature})) -> int => value.v15 }}");
        var c = Compilation.Create("TupleTarget", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var model = c.GetSemanticModel(tree);
        var method = Assert.IsAssignableFrom<MethodSymbol>(model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single()));
        var tuple = Assert.IsAssignableFrom<TupleTypeSymbol>(method.Parameters.Single().Type);
        Assert.Equal(names, tuple.TupleElements.Select(field => field.Name));
        Assert.Equal(target == TargetPlatform.NeoCLR, tuple is Raven.CodeAnalysis.Symbols.StructuralTupleTypeSymbol);
        Assert.Empty(model.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
    }

    [Theory]
    [InlineData(7)]
    [InlineData(8)]
    [InlineData(15)]
    public void SourceTuplePreservesLogicalArity(int size)
    {
        var signature = string.Join(", ", Enumerable.Range(1, size).Select(i => $"v{i}: int"));
        var literal = string.Join(", ", Enumerable.Range(1, size).Select(i => $"v{i}: {i}"));
        var tree = SyntaxTree.ParseText($"public class SourceTuple {{ static func Make() -> ({signature}) => ({literal}) }}");
        var refs = TestMetadataReferences.Default;
        var compilation = Compilation.Create("SourceTuples", [tree], refs, CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream(); var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, refs);
        var value = Assert.IsAssignableFrom<System.Runtime.CompilerServices.ITuple>(loaded.Assembly.GetType("SourceTuple")!.GetMethod("Make")!.Invoke(null, null));
        Assert.Equal(size, value.Length);
        Assert.Equal(size, value[size - 1]);
        var cs = CS.CSharpCompilation.Create("Consumer", [CS.CSharpSyntaxTree.ParseText($"class Consumer {{ public static int Read() => SourceTuple.Make().v{size}; }}")],
            refs.OfType<PortableExecutableReference>().Select(r => CSApi.MetadataReference.CreateFromFile(r.FilePath!)).Append(CSApi.MetadataReference.CreateFromImage(image.ToArray())),
            new CS.CSharpCompilationOptions(CSApi.OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(cs.GetDiagnostics().Where(d => d.Severity == CSApi.DiagnosticSeverity.Error));
    }
    [Theory]
    [InlineData("Location", "line")]
    [InlineData("Location", "line", true)]
    [InlineData("NestedWide", "eighth.right", true)]
    [InlineData("Nested", "inner.number")]
    [InlineData("LeftNested", "last")]
    [InlineData("MixedNames", "result.number")]
    [InlineData("Wide", "eighth")]
    [InlineData("NestedWide", "eighth.right")]
    [InlineData("Fifteen", "last")]
    public void CSharpCanUseTupleNameThroughRavenField(string factory, string access, bool property = false)
    {
        var syntax = SyntaxTree.ParseText($$"""
            import Raven.CodeAnalysis.Tests.TupleFixture.*
            public class Relay { public static {{(property ? "val" : "field")}} Data = {{factory}}() }
            """);
        var refs = TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(TupleFixture).Assembly.Location)).ToArray();
        var c = Compilation.Create("Relay", [syntax], refs, CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream(); var emitted = c.Emit(stream);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var cs = CS.CSharpCompilation.Create("Consumer", [CS.CSharpSyntaxTree.ParseText($"class Consumer {{ public static int Read() => Relay.Data.{access}; }}")],
            refs.OfType<PortableExecutableReference>().Select(r => CSApi.MetadataReference.CreateFromFile(r.FilePath!)).Append(CSApi.MetadataReference.CreateFromImage(stream.ToArray())),
            new CS.CSharpCompilationOptions(CSApi.OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(cs.GetDiagnostics().Where(d => d.Severity == CSApi.DiagnosticSeverity.Error));
    }
}
