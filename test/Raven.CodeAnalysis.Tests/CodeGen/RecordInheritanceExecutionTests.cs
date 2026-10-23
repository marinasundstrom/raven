using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class RecordInheritanceExecutionTests
{
    [Theory]
    [InlineData("abstract override func ToString() -> string")]
    [InlineData("override func ToString() -> string => \"base\"")]
    public void ClosedRecordHierarchyPreservesAuthoredStringDispatch(string baseMember)
    {
        var tree = SyntaxTree.ParseText($$"""
            sealed record Platform permits Desktop, Mobile {
                {{baseMember}}
            }
            record Desktop(Build: int) : Platform {
                override func ToString() -> string => "Desktop ${Build}"
            }
            record Mobile : Platform {
                override func ToString() -> string => "Mobile"
            }
            public class Probe {
                static func Run() -> bool {
                    let desktop: Platform = Desktop(42)
                    let mobile: Platform = Mobile()
                    let boxed: System.Object = desktop
                    let boxedMobile: System.Object = mobile
                    return desktop.ToString() == "Desktop 42" && boxed.ToString() == "Desktop 42"
                        && mobile.ToString() == "Mobile" && boxedMobile.ToString() == "Mobile"
                        && "${desktop}" == "Desktop 42" && "${mobile}" == "Mobile"
                }
            }
            """);
        var compilation = Compilation.Create("ClosedRecordDisplay", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(true, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

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
                    let clone = Item(Item(9, "copy"))
                    let root = Entity(1)
                    let contract: System.IEquatable<Entity> = first
                    return clone.Id == 9 && clone.Name == "copy" && first.Equals(same) && first == same && first != changed && !first.Equals(changed)
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
