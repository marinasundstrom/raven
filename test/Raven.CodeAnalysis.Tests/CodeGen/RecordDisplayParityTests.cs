using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public class RecordDisplayParityTests
{
    public static IEnumerable<object[]> Cases()
    {
        yield return ["record class Empty", "public record Empty;", null!];
        yield return ["record class Empty()", "public record Empty();", null!];
        yield return ["record class Generic<T>(Value: T)", "public record Generic<T>(T Value);", new object[] { 42 }];
        yield return ["record struct Point(X: int, Y: int)", "public readonly record struct Point(int X, int Y);", new object[] { 1, 2 }];
        yield return ["record class Data(Name: string, Count: int)", "public record Data(string Name, int Count);", new object[] { "Ada", 3 }];
        yield return ["""
            record class Data {
                private field _hidden: int = 9
                val Name: string { get; init; } = "Ada"
                val Missing: string? { get; init; } = null
                val Computed: int => 42
                static val Ignored: int = 7
            }
            """, """
            public record Data {
                private int _hidden = 9;
                public string Name { get; init; } = "Ada";
                public string? Missing { get; init; }
                public int Computed => 42;
                public static int Ignored => 7;
            }
            """, null!];
        yield return ["""
            record class Custom {
                private func PrintMembers(builder: System.Text.StringBuilder) -> bool {
                    builder.Append("custom")
                    return true
                }
            }
            """, """
            public sealed record Custom {
                private bool PrintMembers(System.Text.StringBuilder builder) { builder.Append("custom"); return true; }
            }
            """, null!];
        yield return ["""
            abstract record class Base(Name: string)
            record class Derived(Name: string, Count: int) : Base(Name)
            """, """
            public abstract record Base(string Name);
            public record Derived(string Name, int Count) : Base(Name);
            """, new object[] { "Ada", 3 }];
        yield return ["""
            abstract record class Base {
                protected virtual func PrintMembers(builder: System.Text.StringBuilder) -> bool {
                    builder.Append("base")
                    return true
                }
            }
            record class Derived(Count: int) : Base
            """, """
            public abstract record Base {
                protected virtual bool PrintMembers(System.Text.StringBuilder builder) { builder.Append("base"); return true; }
            }
            public record Derived(int Count) : Base;
            """, new object[] { 2 }];
    }

    [Theory]
    [MemberData(nameof(Cases))]
    public void DisplayAndDeconstructionMatchCSharp(string raven, string csharp, object[]? arguments = null)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0")).ToArray();
        var compilation = Compilation.Create("RecordDisplayRaven" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(raven)], paths.Select(path => MetadataReference.CreateFromFile(path)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var reference = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("RecordDisplayCSharp" + Guid.NewGuid().ToString("N"),
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(csharp)],
            paths.Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var referenceImage = new MemoryStream();
        var referenceResult = reference.Emit(referenceImage);
        Assert.True(referenceResult.Success, string.Join(Environment.NewLine, referenceResult.Diagnostics));
        using var expectedLoaded = TestAssemblyLoader.LoadFromStream(referenceImage, compilation.References);
        var expectedType = expectedLoaded.Assembly.GetTypes().Single(type => type.IsPublic && !type.IsAbstract);
        var actualType = loaded.Assembly.GetType(expectedType.Name)!;
        if (expectedType.IsGenericTypeDefinition)
        {
            expectedType = expectedType.MakeGenericType(typeof(int));
            actualType = actualType.MakeGenericType(typeof(int));
        }
        var expected = Activator.CreateInstance(expectedType, arguments ?? []);
        var actual = Activator.CreateInstance(actualType, arguments ?? []);
        Assert.Equal(expected!.ToString(), actual!.ToString());
        Assert.Equal(expectedType.GetMethods().Where(m => m.Name == "Deconstruct").Select(m => m.GetParameters().Length),
            actualType.GetMethods().Where(m => m.Name == "Deconstruct").Select(m => m.GetParameters().Length));
    }
}
