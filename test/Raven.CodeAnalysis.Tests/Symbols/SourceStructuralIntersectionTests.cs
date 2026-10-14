using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class SourceStructuralIntersectionTests
{
    [Theory]
    [InlineData("open class Animal {}", true)]
    [InlineData("abstract class Animal {}", true)]
    [InlineData("sealed class Animal {}", true)]
    [InlineData("class Animal {}", false)]
    [InlineData("struct Animal {}", false)]
    public void NativeIntersection_RequiresExtendableBounds(string declaration, bool valid)
    {
        var tree = SyntaxTree.ParseText(declaration + """

            interface Quacks {}
            class Box<T> {}
            func Echo(value: Animal & Quacks) -> Animal & Quacks { return value }
            func Wrap(value: Box<Animal & Quacks>) -> Box<Animal & Quacks> { return value }
            """);
        var compilation = Compilation.Create("source-intersections", [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithTargetPlatform(TargetPlatform.NeoCLR)
                .WithMetadataImportOptions(new MetadataImportOptions().WithStructuralCompositions()));
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<IntersectionTypeSyntax>())
        {
            var type = model.GetTypeInfo(syntax).Type;
            if (valid)
            {
                var intersection = Assert.IsAssignableFrom<IIntersectionTypeSymbol>(type);
                Assert.Equal(2, intersection.ConstituentTypes.Length);
                Assert.True(SymbolEqualityComparer.Default.Equals(type, model.GetTypeInfo(syntax).Type));
            }
            else
                Assert.False(type is IIntersectionTypeSymbol);
        }
        if (!valid)
            Assert.Contains(compilation.GetSemanticModel(tree).GetDiagnostics(), d => d.Id == "RAV0363");
    }

    [Theory]
    [InlineData(TargetPlatform.DotNet, true)]
    [InlineData(TargetPlatform.NeoCLR, false)]
    public void IntersectionStorage_RemainsOptIn(TargetPlatform target, bool enabled)
    {
        var tree = SyntaxTree.ParseText("interface A {} interface B {} func Echo(value: A & B) -> A & B { return value }");
        var compilation = Compilation.Create("gated-intersections", [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target)
                .WithMetadataImportOptions(new MetadataImportOptions().WithStructuralCompositions(enabled)));
        Assert.Contains(compilation.GetSemanticModel(tree).GetDiagnostics(), d => d.Id == "RAV0363");
    }
}
