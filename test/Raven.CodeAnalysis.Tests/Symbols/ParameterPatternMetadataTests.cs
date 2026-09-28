using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ParameterPatternMetadataTests
{
    [Theory]
    [InlineData("(x, y)", "(int, int)", "(x, y)")]
    [InlineData("[head, ..tail]", "int[3]", "[head, ..tail]")]
    [InlineData("(x, [..items])", "(int, int[])", "(x, [..items])")]
    [InlineData("(x, _)", "(int, int)", "(x, _)")]
    [InlineData("(x /* implementation comment */, y)", "(int, int)", "(x, y)")]
    [InlineData("_", "int", "_")]
    [InlineData("(@class, value)", "(int, int)", "(@class, value)")]
    public void Pattern_RoundTripsIntoImportedSignature(string pattern, string type, string expectedPattern)
    {
        var source = $"public class C {{ public static func M({pattern}: {type}) -> int => 1 }}";
        var references = TestMetadataReferences.Default;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("Patterns", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references).AddSyntaxTrees(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax));
        Assert.NotNull(parameter.BindingPattern);
        var expected = $"{expectedPattern}: {type}";
        Assert.Equal(expected, parameter.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));
        using (var loaded = TestAssemblyLoader.LoadFromStream(stream, references))
        {
            var metadataParameter = Assert.Single(loaded.Assembly.GetType("C")!.GetMethod("M")!.GetParameters());
            var attribute = Assert.Single(metadataParameter.GetCustomAttributesData().Where(a =>
                a.AttributeType.FullName == ParameterPatternFacts.AttributeMetadataName));
            Assert.Equal(1, attribute.ConstructorArguments[0].Value);
            Assert.Equal(expectedPattern, attribute.ConstructorArguments[1].Value);
        }

        var consumer = Compilation.Create("Consumer", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references).AddReferences(MetadataReference.CreateFromImage(stream.ToArray()));
        var method = Assert.Single(consumer.GetTypeByMetadataName("C")!.GetMembers("M").OfType<IMethodSymbol>());
        var imported = Assert.Single(method.Parameters);
        Assert.NotNull(imported.BindingPattern);
        Assert.Equal(parameter.BindingPattern!.Kind, imported.BindingPattern!.Kind);
        Assert.Equal(expected, imported.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));
    }

    [Theory]
    [InlineData("Row(let x)", "Row(let x)")]
    [InlineData("{Value: let x}", "{ Value: let x }")]
    public void StructuralPattern_PreservesImportedSignature(string pattern, string display)
    {
        var source = $"public record class Row(Value: int)\npublic class C {{ public static func Read({pattern}: Row) -> int => x }}";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("NominalPatterns", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references).AddSyntaxTrees(SyntaxTree.ParseText(source));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));
        var consumer = Compilation.Create("Consumer", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references).AddReferences(MetadataReference.CreateFromImage(stream.ToArray()));
        foreach (var current in new[] { compilation, consumer })
        {
            var method = Assert.Single(current.GetTypeByMetadataName("C")!.GetMembers("Read").OfType<IMethodSymbol>());
            var parameter = Assert.Single(method.Parameters);
            Assert.NotNull(parameter.BindingPattern);
            Assert.Equal($"{display}: Row", parameter.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));
        }
    }

    [Fact]
    public void GenericPattern_PreservesBindingsWithSubstitutedTypes()
    {
        const string source = "public class C<T> { public static func M<U>((left, right): (T, U)) -> int => 1 }";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("GenericPatterns", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references).AddSyntaxTrees(SyntaxTree.ParseText(source));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));
        var consumer = Compilation.Create("Consumer", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references).AddReferences(MetadataReference.CreateFromImage(stream.ToArray()));
        foreach (var current in new[] { compilation, consumer })
        {
            var type = current.GetTypeByMetadataName("C`1")!.Construct(current.GetSpecialType(SpecialType.System_Int32));
            var method = Assert.Single(type.GetMembers("M").OfType<IMethodSymbol>())
                .Construct(current.GetSpecialType(SpecialType.System_String));
            var parameter = Assert.Single(method.Parameters);
            Assert.Equal("(left, right): (int, string)", parameter.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));
            Assert.Equal("(int, string)", parameter.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat
                .WithParameterOptions(SymbolDisplayParameterOptions.IncludeType)));
        }
    }

    [Theory]
    [InlineData(2, "(x, y)")]
    [InlineData(1, "(x,")]
    [InlineData(1, "(x, y) extra")]
    [InlineData(1, "")]
    public void UnsupportedOrMalformedMetadata_HasNoPattern(int version, string text)
        => Assert.Null(ParameterPatternFacts.Decode(version, text));
}
