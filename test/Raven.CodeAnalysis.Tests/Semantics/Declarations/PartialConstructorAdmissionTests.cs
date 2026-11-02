using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class PartialConstructorAdmissionTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void StaticConstructorInAnotherPart_DoesNotSuppressImplicitDefault(bool reverseTrees)
    {
        var trees = new[]
        {
            SyntaxTree.ParseText("partial class Child { static init() { } }"),
            SyntaxTree.ParseText("partial class Child { }")
        };
        var compilation = Compilation.Create("PartialStaticConstructor", reverseTrees ? trees.Reverse().ToArray() : trees,
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var constructor = Assert.Single(compilation.GetTypeByMetadataName("Child")!.Constructors.Where(c => !c.IsStatic));
        Assert.Empty(constructor.Parameters);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void ExplicitConstructorInAnotherPart_SuppressesImplicitDefault(bool reverseTrees, bool primaryConstructor)
    {
        var sources = new[]
        {
            "open class Parent { init(value: int) { } }",
            primaryConstructor
                ? "partial class Child(value: int) : Parent(value) { }"
                : "partial class Child : Parent { init(value: int) : base(value) { } }",
            "partial class Child : Parent { }"
        };
        var trees = sources.Select(source => SyntaxTree.ParseText(source)).ToArray();
        var compilation = Compilation.Create("PartialConstructorAdmission", reverseTrees ? trees.Reverse().ToArray() : trees,
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var child = compilation.GetTypeByMetadataName("Child")!;
        var constructor = Assert.Single(child.Constructors.Where(c => !c.IsStatic));
        Assert.Single(constructor.Parameters);
    }
}
