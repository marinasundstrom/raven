using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics;

public sealed class InheritedInterfaceLookupTests
{
    private const string Source = """
        interface Parent<T> {
            val Value: T { get; }
            func Read() -> T
        }
        interface Left<T> : Parent<T> {}
        interface Right<T> : Parent<T> {}
        interface Child : Left<int>, Right<int> {
            val Copy: int => Value
            val self[index: int]: int => Value + index
            func Twice() -> int => Read() + Read()
        }
        class Implementation : Child {
            val Value: int => 21
            func Read() -> int => Value
        }
        class Runner {
            static func Run() -> int {
                let value: Child = Implementation()
                return value.Copy + value.Twice() + value[0]
            }
        }
        """;

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNetInheritedMembersBindAndDispatchFromDefaultBodies()
    {
        var compilation = CreateCompilation(TargetPlatform.DotNet);
        AssertBindings(compilation);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        Assert.Equal(84, assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrInheritedMembersBindFromDefaultBodies()
    {
        // Host references validate shared semantics, not native execution.
        AssertBindings(CreateCompilation(TargetPlatform.NeoCLR));
    }

    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("interface Parent { private func Read() -> int => 1 }\ninterface Child : Parent { func Copy() -> int => Read() }")]
    [InlineData("interface Parent { func Read() -> int => 1 }\nclass Child : Parent { func Copy() -> int => Read() }")]
    public void InheritanceDoesNotExposePrivateHelpersOrDefaultMembersOnClasses(string source)
    {
        var compilation = Compilation.Create("UnavailableInterfaceMember", [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    private static Compilation CreateCompilation(TargetPlatform target)
        => Compilation.Create("InheritedInterfaceLookup", [SyntaxTree.ParseText(Source)],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));

    private static void AssertBindings(Compilation compilation)
    {
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var child = tree.GetRoot().DescendantNodes().OfType<InterfaceDeclarationSyntax>()
            .Single(node => node.Identifier.ValueText == "Child");
        var uses = child.DescendantNodes().OfType<IdentifierNameSyntax>()
            .Where(node => node.Identifier.ValueText is "Value" or "Read").ToArray();
        Assert.Equal(4, uses.Length);
        foreach (var use in uses)
        {
            var symbol = model.GetSymbolInfo(use).Symbol;
            Assert.NotNull(symbol);
            Assert.Equal("Parent", symbol.ContainingType!.Name);
            var type = symbol is PropertySymbol property ? property.Type : ((MethodSymbol)symbol).ReturnType;
            Assert.Equal(SpecialType.System_Int32, type.SpecialType);
        }
    }
}
