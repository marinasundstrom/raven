using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class RecordCopyMetadataParityTests
{
    [Theory]
    [InlineData("public record class Data(Value: int)", "public sealed record Data(int Value);")]
    [InlineData("public open record class Data(Value: int)", "public record Data(int Value);")]
    [InlineData("public open record class Base(Value: int); public record class Data(Other: int) : Base(1)",
        "public record Base(int Value); public sealed record Data(int Other) : Base(1);")]
    [InlineData("public record class Data { public required var Value: int { get; init; } }",
        "public sealed record Data { public required int Value { get; init; } }")]
    [InlineData("public open record class Base { public required var Value: int { get; init; } }; public record class Data : Base",
        "public record Base { public required int Value { get; init; } } public sealed record Data : Base;")]
    public void ReferenceRecordCopyConstructorMatchesCSharp(string raven, string csharp)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var compilation = Compilation.Create("CopyRaven" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(raven)],
            paths.Select(path => MetadataReference.CreateFromFile(path)).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var actual = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var reference = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("CopyCSharp" + Guid.NewGuid().ToString("N"),
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(csharp)],
            paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var referenceImage = new MemoryStream();
        var referenceResult = reference.Emit(referenceImage);
        Assert.True(referenceResult.Success, string.Join(Environment.NewLine, referenceResult.Diagnostics));
        using var expected = TestAssemblyLoader.LoadFromStream(referenceImage, compilation.References);
        var expectedConstructor = CopyConstructor(expected.Assembly.GetType("Data")!);
        var actualConstructor = CopyConstructor(actual.Assembly.GetType("Data")!);
        Assert.Equal(expectedConstructor.Attributes & MethodAttributes.MemberAccessMask,
            actualConstructor.Attributes & MethodAttributes.MemberAccessMask);
        var dataSymbol = compilation.GetTypeByMetadataName("Data")!;
        var symbolCopy = dataSymbol.InstanceConstructors.Single(constructor => constructor.Parameters.Length == 1 &&
            SymbolEqualityComparer.Default.Equals(constructor.Parameters[0].Type, dataSymbol));
        Assert.Equal(HasRequiredMembersMarker(expectedConstructor), symbolCopy.SetsRequiredMembers);
        Assert.Equal(HasRequiredMembersMarker(expectedConstructor), HasRequiredMembersMarker(actualConstructor));
    }

    [Theory]
    [InlineData("public record class Data(Value: int)")]
    [InlineData("public open record class Data(Value: int)")]
    public void SynthesizedCopyConstructorIsNotCallableFromUnrelatedCode(string declaration)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var tree = SyntaxTree.ParseText(declaration + "\nclass Probe { static func Copy() -> Data => Data(Data(1)) }");
        var compilation = Compilation.Create("CopyAccessibility", [tree],
            paths.Select(path => MetadataReference.CreateFromFile(path)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0500");
    }

    private static ConstructorInfo CopyConstructor(Type type)
        => Assert.Single(type.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance),
            constructor => constructor.GetParameters() is [{ ParameterType: var parameterType }] && parameterType == type);

    private static bool HasRequiredMembersMarker(ConstructorInfo constructor)
        => constructor.CustomAttributes.Any(attribute => attribute.AttributeType.FullName == "System.Diagnostics.CodeAnalysis.SetsRequiredMembersAttribute");
}
