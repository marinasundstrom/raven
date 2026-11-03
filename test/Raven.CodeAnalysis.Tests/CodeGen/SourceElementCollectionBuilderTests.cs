using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class SourceElementCollectionBuilderTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("interface Item {}", "class Value : Item {}", "Item")]
    [InlineData("", "record class Value(Number: int)", "Value")]
    public void SpreadBuilderAcceptsSourceElementTypes(string contract, string declaration, string element)
    {
        var construction = element == "Item" ? "Value()" : "Value(42)";
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Generic.*
            import System.Collections.Immutable.*
            {{contract}}
            {{declaration}}
            interface Contract {
                func Read() -> IEnumerable<{{element}}>
                func Items() -> ImmutableArray<{{element}}> => [... Read()]
            }
            class Provider : Contract {
                func Read() -> IEnumerable<{{element}}> => [{{construction}}, {{construction}}]
            }
            class Runner {
                static func Run() -> int {
                    let provider: Contract = Provider()
                    return provider.Items().Length
                }
            }
            """);
        var compilation = Compilation.Create("SourceElementBuilder", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        Assert.Equal(2, Assembly.Load(image.ToArray()).GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }
    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrSpreadBuilderBindsWithHostReferences()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            import System.Collections.Immutable.*
            interface Item {}
            func Copy(items: IEnumerable<Item>) -> ImmutableArray<Item> => [... items]
            """);
        var compilation = Compilation.Create("NeoClrSpreadBuilder", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        // This exercises semantic binding, not native span/runtime support.
        Assert.DoesNotContain(compilation.GetSemanticModel(tree).GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void SpreadBuilderRejectsIncompatibleElements()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Immutable.*
            func Copy(items: string[]) -> ImmutableArray<int> => [... items]
            """);
        var compilation = Compilation.Create("InvalidSpreadBuilder", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV1503");
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void ExplicitSpanArgumentToParamsBuilderPreservesElements()
    {
        var tree = SyntaxTree.ParseText("""
            import System.*
            import System.Collections.Immutable.*
            class Runner {
                static func Run() -> int {
                    let values: ReadOnlySpan<int> = [10, 20]
                    return ImmutableArray.Create<int>(values).Length
                }
            }
            """);
        var compilation = Compilation.Create("ExplicitSpanBuilder", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        Assert.Equal(2, Assembly.Load(image.ToArray()).GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

}
