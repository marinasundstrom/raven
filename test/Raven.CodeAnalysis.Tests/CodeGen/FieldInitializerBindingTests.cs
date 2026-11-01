using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class FieldInitializerBindingTests : CompilationTestBase
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void ForwardFactoryInitializerIsCompletedBeforeEmission(bool inspectLastFieldFirst, bool separatePart)
    {
        const string declaration = """
        public static partial class Values {
            private static readonly field source: int[] = CreateSource()
            static readonly field count: int = source.Length
        }
        """;
        const string factory = """
        public static partial class Values {
            private static func CreateSource() -> int[] => [42]
            static func Read() -> int => count
        }
        """;
        var tree = SyntaxTree.ParseText(separatePart ? declaration : declaration + "\n" + factory);
        var compilation = separatePart
            ? CreateCompilation([tree, SyntaxTree.ParseText(factory)])
            : CreateCompilation(tree);
        if (inspectLastFieldFirst)
        {
            var model = compilation.GetSemanticModel(tree);
            foreach (var identifier in tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>().Reverse())
                _ = model.GetSymbolInfo(identifier);
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(1, loaded.Assembly.GetType("Values")!.GetMethod("Read")!.Invoke(null, null));
    }

    [Fact]
    public void UnresolvedInitializerStillReportsAnError()
    {
        var (compilation, _) = CreateCompilation("""
        class Invalid {
            static readonly field value: int = Missing()
        }
        """);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0103");
        using var image = new MemoryStream();
        Assert.False(compilation.Emit(image).Success);
    }

}
