using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen.Functions.Parameters;

public sealed class ByRefParameterCodeGenTests
{
    [Fact]
    [Trait("Target", "DotNet")]
    public void ReadOnlyOutputDeclarationInInitializerRemainsInScope()
    {
        const string code = """
            class Program {
                static func Run() -> int {
                    let quotient = System.Math.DivRem(85, 43, out let remainder)
                    return quotient + remainder
                }
            }
            """;
        Assert.Equal(43, CompileAndInvokeRun<int>(code, "readonly-out-initializer"));
    }

    [Theory]
    [InlineData("let", false)]
    [InlineData("val", false)]
    [InlineData("let", true)]
    [InlineData("val", true)]
    [InlineData("var", false)]
    [InlineData("var", true)]
    [Trait("Target", "DotNet")]
    public void OutDeclarationIsInitializedByDeclaringCall(string binding, bool metadataCall)
    {
        var invocation = metadataCall
            ? $"System.Math.DivRem(85, 43, out {binding} value)"
            : $"if !Set(out {binding} value) {{ return -1 }}";
        var code = $$"""
            class Program {
                static func Set(out value: int) -> bool { value = 42; return true }
                static func Run() -> int {
                    {{invocation}}
                    return value
                }
            }
            """;
        Assert.Equal(42, CompileAndInvokeRun<int>(code, $"readonly-out-{binding}-{metadataCall}"));
    }

    [Theory]
    [InlineData("let")]
    [InlineData("val")]
    [InlineData("var")]
    [Trait("Target", "DotNet")]
    public void OutDeclarationTakesTypeFromSelectedGenericMethod(string binding)
    {
        var code = $$"""
            class Program {
                static func Set<T>(input: T, out value: T) { value = input }
                static func Run() -> int {
                    Set(42, out {{binding}} value)
                    return value
                }
            }
            """;
        Assert.Equal(42, CompileAndInvokeRun<int>(code, $"inferred-out-{binding}"));
    }

    [Theory]
    [InlineData("let")]
    [InlineData("var")]
    [Trait("Target", "DotNet")]
    public void LocalInitializer_ForwardedOutCallAssignsParameter(string binding)
    {
        var code = $$"""
            interface Scanner { func Read(out done: bool) -> int }
            class Reader : Scanner {
                func Read(out done: bool) -> int { done = true; return 42 }
            }
            class Program {
                static func Forward(value: object, out done: bool) -> int {
                    if value is not Scanner scanner { throw System.InvalidOperationException() }
                    {{binding}} token = scanner.Read(out done)
                    return token
                }
                static func Run() -> int {
                    let token = Forward(Reader(), out var done)
                    return if done { token } else { -1 }
                }
            }
            """;
        Assert.Equal(42, CompileAndInvokeRun<int>(code, "out-initializer-" + binding));
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void RefArgument_CanBeReadByInParameter()
    {
        const string code = """
            class Program {
                static func Read(in value: int) -> int => value
                static func Run() -> int {
                    var value = 42
                    return Read(ref value)
                }
            }
            """;
        Assert.Equal(42, CompileAndInvokeRun<int>(code, "ref-to-in-runtime"));
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void VolatileRead_RefIntSelectsNumericOverload()
    {
        const string code = """
            import System.Threading.*
            class Program {
                private static field value: int = 0
                static func Run() -> int {
                    Volatile.Write(ref value, 42)
                    return Volatile.Read(ref value)
                }
            }
            """;
        Assert.Equal(42, CompileAndInvokeRun<int>(code, "volatile-read-runtime"));
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void AssignedOutParameter_CopiesValueWithoutAliasingDestination()
    {
        const string code = """
            class Program {
                static func Copy(out first: int, out second: int) {
                    first = 21
                    second = first
                    first = 100
                }
                static func Run() -> int {
                    var first = 0
                    var second = 0
                    Copy(out first, out second)
                    return first + second
                }
            }
            """;
        Assert.Equal(121, CompileAndInvokeRun<int>(code, "out-value-copy-runtime"));
    }

    [Fact]
    public void ForwardedOutParameters_AreAssignedBySourceAndMetadataCalls()
    {
        const string code = """
class Program {
    static func Set<T>(out value: T, payload: T) { value = payload }
    static func Forward(out value: int) { Set<int>(out value, 20) }
    static func Remainder(out value: int) {
        System.Math.DivRem(45, 23, out value)
        return
    }
    public static func Run() -> int {
        var first = 0
        var second = 0
        Forward(out first)
        Remainder(out second)
        return first + second
    }
}
""";
        Assert.Equal(42, CompileAndInvokeRun<int>(code, "forwarded-out-runtime"));
    }

    [Fact]
    public void ReturnedGenericOutCall_AssignsForwardedParameter()
    {
        const string code = """
class Program {
    static func Set<T>(out value: T) -> bool {
        value = default
        return true
    }
    static func Forward<T>(out value: T) -> bool {
        return Set<T>(out value)
    }
    public static func Run() -> int {
        var value = -1
        if !Forward<int>(out value) { return -1 }
        return value + 42
    }
}
""";
        Assert.Equal(42, CompileAndInvokeRun<int>(code, "returned-generic-out-runtime"));
    }

    [Fact]
    public void SourceOutParameter_WithDeclaredVarLocal_EmitsCallAndAssignedLocal()
    {
        const string code = """
class Program {
    static func TryParse(text: string, out result: int) -> bool {
        result = 41
        return true
    }

    public static func Run() -> int {
        if !TryParse("value", out var total) {
            return -1
        }

        return total + 1
    }
}
""";

        var value = CompileAndInvokeRun<int>(code, "source-out-var-runtime");

        Assert.Equal(42, value);
    }

    [Fact]
    public void MetadataOutParameter_WithDeclaredVarLocal_EmitsCallAndAssignedLocal()
    {
        const string code = """
import System.Text.Json.*

class Program {
    public static func Run() -> int {
        use doc = JsonDocument.Parse("{ \"value\": 42 }")
        let root = doc.RootElement

        if !root.TryGetProperty("value", out var property) {
            return -1
        }

        return property.GetInt32()
    }
}
""";

        var value = CompileAndInvokeRun<int>(code, "metadata-out-var-runtime");

        Assert.Equal(42, value);
    }

    private static T CompileAndInvokeRun<T>(string code, string assemblyName)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        return (T)run.Invoke(null, Array.Empty<object>())!;
    }
}
