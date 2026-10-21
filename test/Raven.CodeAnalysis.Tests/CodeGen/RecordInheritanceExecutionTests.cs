using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class RecordInheritanceExecutionTests
{
    [Fact]
    public void BaseTypedEqualityObservesRuntimeTypeAndDerivedState()
    {
        var tree = SyntaxTree.ParseText("""
            open record Entity(Id: int)
            record Item(Id: int, Name: string) : Entity(Id)
            record Other(Id: int) : Entity(Id)
            record Leaf(Name: string) : Entity(7)
            public class Probe {
                public static func Run() -> bool {
                    let first: Entity = Item(1, "a")
                    let same: Entity = Item(1, "a")
                    let changed: Entity = Item(1, "b")
                    let unrelated: Entity = Other(1)
                    let root = Entity(1)
                    let contract: System.IEquatable<Entity> = first
                    return first.Equals(same) && first == same && first != changed && !first.Equals(changed)
                        && first != unrelated && unrelated != first && first != root && root != first
                        && first.GetHashCode() == same.GetHashCode()
                        && contract.Equals(same) && !contract.Equals(changed)
                        && Leaf("x").ToString() == "Leaf { Id = 7, Name = \"x\" }"
                }
            }
            """);
        var compilation = Compilation.Create("RecordInheritance", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(true, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
