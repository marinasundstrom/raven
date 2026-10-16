using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class CheckedMutationTests
{
    [Theory]
    [InlineData("field")]
    [InlineData("var")]
    public void QualifiedStaticMutationDoesNotEvaluateTypeReceiver(string storage)
    {
        using var loaded = Load($$"""
            class Counter {
                public static {{storage}} Count: int
            }
            class Runner {
                static func Run() -> int {
                    Counter.Count = 40
                    checked(Counter.Count += 1)
                    checked(Counter.Count += 1)
                    return Counter.Count
                }
            }
            """);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("int", "x += 1", int.MaxValue)]
    [InlineData("int", "x -= 1", int.MinValue)]
    [InlineData("int", "x *= 2", int.MaxValue)]
    [InlineData("uint", "x += (uint)1", uint.MaxValue)]
    [InlineData("long", "x++", long.MaxValue)]
    [InlineData("ulong", "++x", ulong.MaxValue)]
    [InlineData("int", "x--", int.MinValue)]
    [InlineData("int", "--x", int.MinValue)]
    [InlineData("byte", "x += (byte)1", byte.MaxValue)]
    [InlineData("byte", "x++", byte.MaxValue)]
    [InlineData("short", "--x", short.MinValue)]
    public void OverflowDoesNotWriteBack(string type, string expression, object value)
    {
        using var loaded = Load($$"""
            class Runner {
                public static var x: {{type}}
                static func Run() { checked({{expression}}) }
            }
            """);
        var runner = loaded.Assembly.GetType("Runner")!;
        var field = runner.GetProperty("x")!;
        field.SetValue(null, value);
        Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() => runner.GetMethod("Run")!.Invoke(null, null)).InnerException);
        Assert.Equal(value, field.GetValue(null));
    }

    [Theory]
    [InlineData("+=")]
    [InlineData("-=")]
    [InlineData("*=")]
    [InlineData("/=")]
    public void InstanceCheckedOperatorsAreSelected(string token)
    {
        using var loaded = Load($$"""
            struct Counter {
                public var Value: int = 0
                func {{token}}(value: int) { Value = 1 }
                func checked {{token}}(value: int) { Value = 42 }
            }
            class Runner {
                static func Run() -> int {
                    var counter = Counter()
                    checked(counter {{token}} 1)
                    return counter.Value
                }
            }
            """);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("++x")]
    [InlineData("--x")]
    public void InstanceCheckedPrefixReturnsUpdatedValue(string expression)
    {
        using var loaded = Load($$"""
            struct Counter {
                public var Value: int = 0
                func ++() { Value = 1 }
                func checked ++() { Value = 42 }
                func --() { Value = 1 }
                func checked --() { Value = 42 }
            }
            class Runner {
                static func Run() -> int {
                    var x = Counter()
                    let result = checked({{expression}})
                    return result.Value
                }
            }
            """);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("checked(unchecked(x += 1))", int.MinValue)]
    [InlineData("unchecked(x++)", int.MinValue)]
    [InlineData("checked(x += unchecked(1))", null)]
    public void MutationUsesItsOwnContext(string expression, object? expected)
    {
        using var loaded = Load($$"""
            class Runner {
                static func Run() -> int {
                    var x = 2147483647
                    {{expression}}
                    return x
                }
            }
            """);
        var method = loaded.Assembly.GetType("Runner")!.GetMethod("Run")!;
        if (expected is null)
            Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() => method.Invoke(null, null)).InnerException);
        else
            Assert.Equal(expected, method.Invoke(null, null));
    }

    [Theory]
    [InlineData("checked(x++)", 42)]
    [InlineData("unchecked(x++)", 1)]
    public void DiscardedPostfixUsesInstanceOperator(string expression, int expected)
    {
        using var loaded = Load($$"""
            class Counter {
                public var Value: int = 0
                func ++() { Value = 1 }
                func checked ++() { Value = 42 }
            }
            class Runner {
                static func Run() -> int {
                    var x = Counter()
                    {{expression}}
                    return x.Value
                }
            }
            """);
        Assert.Equal(expected, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("checked(ItemsOnce()[IndexOnce()] += 2)", 44)]
    [InlineData("checked(++ItemsOnce()[IndexOnce()])", 43)]
    [InlineData("checked(ItemsOnce()[IndexOnce()]++)", 43)]
    public void ArrayReceiverAndIndexAreEvaluatedOnce(string expression, int expected)
    {
        using var loaded = Load($$"""
            class Runner {
                static var Calls: int = 0
                static var Items: int[] = [40]
                static func ItemsOnce() -> int[] { Calls = Calls + 1; return Items }
                static func IndexOnce() -> int { Calls = Calls + 1; return 0 }
                static func Run() -> int {
                    {{expression}}
                    return Items[0] + Calls
                }
            }
            """);
        Assert.Equal(expected, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("checked(x++)", 40)]
    [InlineData("checked(++x)", 42)]
    public void StaticIncrementUsesCheckedOverloadAndPreservesResult(string expression, int expected)
    {
        using var loaded = Load($$"""
            class Counter {
                public var Value: int = 40
                static func ++(value: Counter) -> Counter => Counter() { Value = 1 }
                static func checked ++(value: Counter) -> Counter => Counter() { Value = 42 }
            }
            class Runner {
                static func Run() -> int {
                    var x = Counter()
                    let result = {{expression}}
                    return result.Value
                }
            }
            """);
        Assert.Equal(expected, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    private static TestAssemblyLoader.LoadedAssembly Load(string source)
    {
        var compilation = Compilation.Create("CheckedMutation_" + Guid.NewGuid().ToString("N"),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        image.Position = 0;
        return TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
    }
}
