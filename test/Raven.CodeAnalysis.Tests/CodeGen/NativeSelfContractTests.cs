using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Mono.Cecil;

namespace Raven.CodeAnalysis.Tests;

public class NativeSelfContractTests
{
    const string Source = """
        public interface Cloneable { func Clone() -> Self; val Current: Self { get; } }
        public interface Comparable<T> { func Compare(other: T) -> int }
        public interface Number : Comparable<Self> {
            static val Zero: Self { get; }
            static func +(left: Self, right: Self) -> Self
            static func Add(left: Self, right: Self) -> Self
        }
        public struct Count : Number, Cloneable {
            public func Clone() -> Self => self
            public val Current: Self => self
            public var Value: int
            public func Compare(other: Self) -> int => Value - other.Value
            static val Zero: Self => Count()
            static func +(left: Self, right: Self) -> Self => Add(left, right)
            public static func Add(left: Self, right: Self) -> Self {
                var result = Count()
                result.Value = left.Value + right.Value
                return result
            }
        }
        public class Consumer {
            public static func Sum<T>(left: T, right: T) -> T where T: Number => T.Add(left, right) + T.Zero
        }
        """;

    private static MetadataReference[] References()
    {
        var marker = Compilation.Create("NativeSelfReference", [SyntaxTree.ParseText("public sealed class NativeSelfMarker {}")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        Assert.True(marker.Emit(image).Success);
        return TestMetadataReferences.Default.Append(MetadataReference.CreateFromImage(image.ToArray())).ToArray();
    }

    [Fact]
    public void NativeSelfKeepsInterfaceArityAndEmitsMarkerAndConstrainedCall()
    {
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeSelfTypeContract(new RuntimeSelfTypeContract("NativeSelfReference", "NativeSelfMarker"))
            .WithAsyncCancellationPropagation(true);
        var compilation = Compilation.Create("NativeSelfProbe", [SyntaxTree.ParseText(Source)], References(), options);
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(!diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("\n", diagnostics));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        stream.Position = 0;
        using var module = ModuleDefinition.ReadModule(stream);
        var number = module.GetType("Number");
        Assert.Empty(number.GenericParameters);
        var count = module.GetType("Count");
        Assert.Contains(count.Interfaces, i => i.InterfaceType.FullName == "Comparable`1<Count>");
        Assert.True(count.Methods.Single(m => m.Name == "Compare").IsVirtual);
        Assert.Equal("NativeSelfMarker", number.Methods.Single(m => m.Name == "Add").ReturnType.FullName);
        Assert.Equal("Count", module.GetType("Count").Methods.Single(m => m.Name == "Add").ReturnType.FullName);
        Assert.Single(module.GetType("Consumer").Methods.Single(m => m.Name == "Sum").GenericParameters);
    }

    [Fact]
    public void ErasedSelfCallAndWrongImplementationAreRejected()
    {
        foreach (var source in new[] {
            "interface Cloneable { func Clone() -> Self } func Bad(value: Cloneable) { value.Clone() }",
            Source.Replace("public static func Add(left: Self, right: Self) -> Self", "public static func Add(left: Self, right: Self) -> string").Replace("return result", "return \"wrong\"")
        })
        {
            var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeSelfTypeContract(new RuntimeSelfTypeContract("NativeSelfReference", "NativeSelfMarker"))
            .WithAsyncCancellationPropagation(true);
            var compilation = Compilation.Create("NativeSelfProbe", [SyntaxTree.ParseText(source)], References(), options);
            Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        }
    }

    [Fact]
    public void OrdinaryClrCompilationDoesNotEnableNativeSelf()
    {
        var compilation = Compilation.Create("Ordinary", [SyntaxTree.ParseText(Source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
