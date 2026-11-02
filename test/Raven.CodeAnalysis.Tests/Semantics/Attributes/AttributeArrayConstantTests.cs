using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class AttributeArrayConstantTests
{
    [Theory]
    [InlineData(0, false)]
    [InlineData(1, false)]
    [InlineData(3, false)]
    [InlineData(8, false)]
    [InlineData(9, false)]
    [InlineData(3, true)]
    public void ParamsArray_PreservesElementsAcrossColdAndDiagnosticsQueries(int count, bool diagnosticsFirst)
    {
        var arguments = string.Join(", ", Enumerable.Range(1, count));
        var tree = SyntaxTree.ParseText($$"""
            class NumbersAttribute : System.Attribute {
                init(params values: int[]) { }
            }
            [Numbers({{arguments}})]
            class Probe { }
            """);
        var compilation = Compilation.Create("ArrayAttribute", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.ValueText == "Probe");
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        var constant = Assert.Single(Assert.Single(symbol.GetAttributes()).ConstructorArguments);
        Assert.Equal(TypedConstantKind.Array, constant.Kind);
        Assert.Equal(Enumerable.Range(1, count), constant.Values.Select(v => Assert.IsType<int>(v.Value)));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void TypeArray_EmitsAndPreservesMetadataValues()
    {
        var tree = SyntaxTree.ParseText("""
            class TypesAttribute : System.Attribute {
                init(params types: System.Type[]) { }
            }
            [Types(typeof(int), typeof(string), typeof(bool))]
            class Probe { }
            """);
        var compilation = Compilation.Create("TypeArrayAttribute", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var attribute = loaded.Assembly.GetType("Probe")!.GetCustomAttributesData().Single(a => a.AttributeType.Name == "TypesAttribute");
        var values = Assert.IsAssignableFrom<IList<System.Reflection.CustomAttributeTypedArgument>>(Assert.Single(attribute.ConstructorArguments).Value);
        Assert.Equal(new[] { typeof(int), typeof(string), typeof(bool) }, values.Select(v => Assert.IsAssignableFrom<Type>(v.Value)));
    }
}
