using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedEmissionParityTests
{
    public static TheoryData<string, int> Programs => new()
    {
        {
            """
            class State {
                public var Trace: int = 0
                public func Next(value: int) -> int {
                    Trace = Trace * 10 + value
                    return value
                }
                public func Receiver() -> State {
                    Next(1)
                    return self
                }
                public func Combine(left: int, right: int) -> int => left * 10 + right
            }
            class Runner {
                public static func Run() -> int {
                    let state = State()
                    let result = state.Receiver().Combine(state.Next(2), state.Next(3))
                    return state.Trace * 100 + result
                }
            }
            """, 12323
        },
        {
            """
            class Runner {
                public static func Run() -> int {
                    var values: int[] = [1, 2, 3]
                    var sum = 0
                    for item in values {
                        values = [9]
                        if item == 2 { continue }
                        sum = sum + item
                    }
                    return sum * 10 + values[0]
                }
            }
            """, 49
        },
        {
            """
            class State {
                public var Calls: int = 0
                public func Tick() -> bool {
                    Calls = Calls + 1
                    return true
                }
            }
            class Runner {
                public static func Run() -> int {
                    let state = State()
                    let first = false && state.Tick()
                    let second = true || state.Tick()
                    let third = true && state.Tick()
                    let fourth = false || state.Tick()
                    if first || !second || !third || !fourth { return -1 }
                    return state.Calls
                }
            }
            """, 2
        }
    };

    [Theory]
    [MemberData(nameof(Programs))]
    public void DebugAndReleasePreserveObservableBehavior(string source, int expected)
    {
        foreach (var optimization in new[] { OptimizationLevel.Debug, OptimizationLevel.Release })
        {
            using var loaded = Compile(source, optimization);
            Assert.Equal(expected, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
        }
    }

    [Theory]
    [InlineData(OptimizationLevel.Debug)]
    [InlineData(OptimizationLevel.Release)]
    public void InstanceCallPreservesNullReceiverFault(OptimizationLevel optimization)
    {
        using var loaded = Compile("""
            class Item {
                public func Answer() -> int => 42
            }
            class Runner {
                public static func Run(item: Item) -> int => item.Answer()
            }
            """, optimization);
        var thrown = Assert.Throws<TargetInvocationException>(() =>
            loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, [null]));
        Assert.IsType<NullReferenceException>(thrown.InnerException);
    }

    private static TestAssemblyLoader.LoadedAssembly Compile(string source, OptimizationLevel optimization)
    {
        var compilation = Compilation.Create("SharedEmissionParity", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        return TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
    }
}
