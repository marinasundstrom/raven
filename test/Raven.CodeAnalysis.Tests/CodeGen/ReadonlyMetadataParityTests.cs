using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public class ReadonlyMetadataParityTests
{
    public static IEnumerable<object[]> RecordCases()
    {
        yield return ["public readonly record struct Data(Value: int)", "public readonly record struct Data(int Value);"];
        yield return ["public record struct Data(var Value: int)", "public record struct Data(int Value);"];
        yield return ["""
            public record struct Data(var Value: int) {
                field Count: int
                val Next: int { get { Count += 1; return Count } }
            }
            """, "public record struct Data(int Value) { public int Count; public int Next => ++Count; }"];
        yield return ["public readonly struct Data { val Value: int { get; } }",
            "public readonly struct Data { public int Value { get; } }"];
    }

    [Theory]
    [MemberData(nameof(RecordCases))]
    public void EmittedReadonlyAndGeneratedMarkersMatchCSharp(string raven, string csharp)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var compilation = Compilation.Create("ReadonlyRaven" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(raven)],
            paths.Select(path => MetadataReference.CreateFromFile(path)).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var actual = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var reference = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("ReadonlyCSharp" + Guid.NewGuid().ToString("N"),
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(csharp)],
            paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var referenceImage = new MemoryStream();
        var referenceResult = reference.Emit(referenceImage);
        Assert.True(referenceResult.Success, string.Join(Environment.NewLine, referenceResult.Diagnostics));
        using var expected = TestAssemblyLoader.LoadFromStream(referenceImage, compilation.References);
        var expectedType = expected.Assembly.GetType("Data")!;
        var actualType = actual.Assembly.GetType("Data")!;
        Assert.Equal(HasReadonly(expectedType), HasReadonly(actualType));
        Assert.Equal(MemberMarkers(expectedType), MemberMarkers(actualType));
        object[] arguments = expectedType.GetConstructors().Any(constructor => constructor.GetParameters().Length == 1) ? [3] : [];
        var expectedValue = Activator.CreateInstance(expectedType, arguments)!;
        var actualValue = Activator.CreateInstance(actualType, arguments)!;
        Assert.Equal(expectedValue.ToString(), actualValue.ToString());
        if (expectedType.GetField("Count") is { } expectedCount)
            Assert.Equal(expectedCount.GetValue(expectedValue), actualType.GetField("Count")!.GetValue(actualValue));
        Assert.Equal(HasReadonly(expectedType), compilation.GetTypeByMetadataName("Data")!.IsReadOnly);
    }

    [Fact]
    public void ImportedReadonlyMethodsMatchCSharpSymbols()
    {
        const string source = """
            public readonly struct ReadonlyValue { public int Read() => 1; public static int StaticRead() => 2; }
            public struct MutableValue {
                private static int storage;
                public readonly int Read() => 1;
                public int Write() => 2;
                public ref readonly int Reference() => ref storage;
            }
            """;
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var reference = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("ReadonlyImport" + Guid.NewGuid().ToString("N"),
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(source)],
            paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        Assert.True(reference.Emit(image).Success);
        var compilation = Compilation.Create("Consumer", [], references:
            paths.Select(path => MetadataReference.CreateFromFile(path)).Append(MetadataReference.CreateFromImage(image.ToArray())).ToArray(),
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        foreach (var name in new[] { "ReadonlyValue", "MutableValue" })
        {
            var expected = reference.GetTypeByMetadataName(name)!;
            var actual = compilation.GetTypeByMetadataName(name)!;
            Assert.Equal(expected.IsReadOnly, actual.IsReadOnly);
            foreach (var method in expected.GetMembers().OfType<Microsoft.CodeAnalysis.IMethodSymbol>().Where(method => method.MethodKind == Microsoft.CodeAnalysis.MethodKind.Ordinary))
                Assert.Equal(method.IsReadOnly, actual.GetMembers(method.Name).OfType<MethodSymbol>().Single().IsReadOnly);
        }
    }

    [Fact]
    public void UnionStorageAndCasesAreReadonly()
    {
        var source = """
            public union State {
                case Fixed(value: int)
                case Missing
            }
            """;
        var compilation = Compilation.Create("ReadonlyUnion", [SyntaxTree.ParseText(source)], GetReferences(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var carrier = loaded.Assembly.GetType("State")!;
        Assert.True(HasReadonly(carrier));
        Assert.All(carrier.GetFields(BindingFlags.Instance | BindingFlags.NonPublic), field => Assert.True(field.IsInitOnly));
        Assert.True(HasReadonly(loaded.Assembly.GetType("State+Fixed")!));
        Assert.True(HasReadonly(loaded.Assembly.GetType("State+Missing")!));
        var property = Assert.Single(loaded.Assembly.GetType("State+Fixed")!.GetProperties());
        Assert.Null(property.SetMethod);
    }

    [Theory]
    [InlineData("readonly record struct Data(var Value: int)", "RAV0341")]
    [InlineData("readonly record class Data(Value: int)", "RAV0332")]
    [InlineData("union State { case Item(var value: int) }", "RAV0342")]
    [InlineData("union State { case Item(val value: int) }", "RAV0342")]
    public void ReadonlyRecordRestrictionsAreDiagnosed(string source, string id)
    {
        var compilation = Compilation.Create("ReadonlyInvalid", [SyntaxTree.ParseText(source)], GetReferences(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == id);
    }

    private static MetadataReference[] GetReferences()
        => TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
            .Select(path => MetadataReference.CreateFromFile(path)).ToArray();

    private static bool HasReadonly(MemberInfo member)
        => member.CustomAttributes.Any(attribute => attribute.AttributeType.FullName == "System.Runtime.CompilerServices.IsReadOnlyAttribute");

    private static string[] MemberMarkers(Type type)
        => type.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly)
            .Select(method => $"{method.Name}({string.Join(",", method.GetParameters().Select(parameter => parameter.ParameterType.Name))})" +
                $" readonly={HasReadonly(method)} generated={method.CustomAttributes.Any(attribute => attribute.AttributeType.FullName == "System.Runtime.CompilerServices.CompilerGeneratedAttribute")}")
            .OrderBy(value => value).ToArray();
}
