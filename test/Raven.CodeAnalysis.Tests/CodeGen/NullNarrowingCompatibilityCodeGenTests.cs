using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class NullNarrowingCompatibilityCodeGenTests
{
    [Fact]
    public void IsNotNullCompatibilityOption_EmitsReferenceAndValueTypeUnwraps()
    {
        const string code = """
class NullNarrowingRuntime {
    static func ValuePresent() -> int {
        let value: int? = 41
        if value is not null {
            return value + 1
        }
        return 0
    }

    static func ValueMissing() -> int {
        let value: int? = null
        if value is not null {
            return value + 1
        }
        return 0
    }

    static func ReferencePresent() -> int {
        let value: string? = "Raven"
        if value is not null {
            return value.Length
        }
        return 0
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithEnableIsNotNullNarrowing(true);
        var compilation = Compilation.Create("null_narrowing_codegen", [syntaxTree], options)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("NullNarrowingRuntime", throwOnError: true)!;
        var flags = BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;

        Assert.Equal(42, (int)type.GetMethod("ValuePresent", flags)!.Invoke(null, null)!);
        Assert.Equal(0, (int)type.GetMethod("ValueMissing", flags)!.Invoke(null, null)!);
        Assert.Equal(5, (int)type.GetMethod("ReferencePresent", flags)!.Invoke(null, null)!);
    }
}
