using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class CovariantOverrideCodeGenTests
{
    [Fact]
    public void CovariantReturnPreservesDispatchAcrossThreeLevels()
    {
        const string source = """
            open class Root { virtual func Copy() -> Root { throw System.Exception("root") } }
            open class Middle : Root { override func Copy() -> Middle { throw System.Exception("middle") } }
            class Leaf : Middle { override func Copy() -> Leaf => self }
            class Probe {
                static func Run() -> bool {
                    let leaf = Leaf()
                    let root: Root = leaf
                    let middle: Middle = leaf
                    return System.Object.ReferenceEquals(root.Copy(), leaf) && System.Object.ReferenceEquals(middle.Copy(), leaf)
                }
            }
            """;
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("CovariantOverride", [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.True(!compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, compilation.GetDiagnostics()));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        Assert.Equal(true, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
        var leaf = loaded.Assembly.GetType("Leaf")!;
        var method = leaf.GetMethod("Copy", System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.DeclaredOnly)!;
        Assert.Equal(leaf, method.ReturnType);
    }

    [Theory]
    [InlineData("int", "1")]
    [InlineData("string", "\"wrong\"")]
    [InlineData("Derived?", "null")]
    public void NonCovariantReturnIsRejected(string returnType, string expression)
    {
        var source = $$"""
            open class Base { virtual func Copy() -> Base => self }
            class Derived : Base { override func Copy() -> {{returnType}} => {{expression}} }
            """;
        var compilation = Compilation.Create("InvalidCovariantOverride", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0307");
    }
}
