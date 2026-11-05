using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class PropertyStorageMetadataParityTests
{
    [Theory]
    [InlineData("class", "val Value: int { get; }", "int Value { get; }")]
    [InlineData("struct", "val Value: int { get; }", "int Value { get; }")]
    [InlineData("readonly struct", "val Value: int { get; }", "int Value { get; }")]
    [InlineData("class", "static val Value: int { get; }", "static int Value { get; }")]
    [InlineData("class", "var Value: int { get; set; }", "int Value { get; set; }")]
    [InlineData("class", "val Value: int { get; init; }", "int Value { get; init; }")]
    [InlineData("class", "val Value: int { get; }\ninit() { Value = 7 }", "int Value { get; } public Data() { Value = 7; }")]
    [InlineData("class", "val Value: int { get; init; }\ninit() { Value = 7 }", "int Value { get; init; } public Data() { Value = 7; }")]
    [InlineData("class", "val Value: int { get => field }", "int Value { get => field; }")]
    [InlineData("class", "val Value: int { get { field += 1; return field } }", "int Value { get { field += 1; return field; } }")]
    public void PropertyBackingFieldMutabilityMatchesCSharp(string kind, string ravenProperty, string csharpProperty)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var references = paths.Select(MetadataReference.CreateFromFile).ToArray();
        var compilation = Compilation.Create("RavenPropertyStorage", [SyntaxTree.ParseText($"public {kind} Data {{ {ravenProperty} }}")],
            references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var actualImage = new MemoryStream();
        var result = compilation.Emit(actualImage);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var actual = TestAssemblyLoader.LoadFromStream(actualImage, references);
        var csharp = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("CSharpPropertyStorage",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText($"public {kind} Data {{ public {csharpProperty} }}", new Microsoft.CodeAnalysis.CSharp.CSharpParseOptions(Microsoft.CodeAnalysis.CSharp.LanguageVersion.Preview))],
            paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var expectedImage = new MemoryStream();
        var expectedResult = csharp.Emit(expectedImage);
        Assert.True(expectedResult.Success, string.Join(Environment.NewLine, expectedResult.Diagnostics));
        using var expected = TestAssemblyLoader.LoadFromStream(expectedImage, references);
        const BindingFlags flags = BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly;
        var expectedField = Assert.Single(expected.Assembly.GetType("Data")!.GetFields(flags));
        var actualField = Assert.Single(actual.Assembly.GetType("Data")!.GetFields(flags));
        Assert.Equal(expectedField.IsInitOnly, actualField.IsInitOnly);
        Assert.Equal(expectedField.IsStatic, actualField.IsStatic);
        Assert.Equal(expectedField.FieldType, actualField.FieldType);
        var expectedType = expected.Assembly.GetType("Data")!;
        var actualType = actual.Assembly.GetType("Data")!;
        var expectedObject = expectedField.IsStatic ? null : Activator.CreateInstance(expectedType);
        var actualObject = actualField.IsStatic ? null : Activator.CreateInstance(actualType);
        var expectedProperty = expectedType.GetProperty("Value")!;
        var actualProperty = actualType.GetProperty("Value")!;
        Assert.Equal(expectedProperty.GetValue(expectedObject), actualProperty.GetValue(actualObject));
        if (expectedProperty.CanWrite)
        {
            expectedProperty.SetValue(expectedObject, 9);
            actualProperty.SetValue(actualObject, 9);
            Assert.Equal(expectedProperty.GetValue(expectedObject), actualProperty.GetValue(actualObject));
        }
    }
}
