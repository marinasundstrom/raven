using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Operations;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class CompoundOperatorTests
{
    [Theory]
    [InlineData("%=", "op_ModulusAssignment")]
    [InlineData("<<=", "op_LeftShiftAssignment")]
    [InlineData(">>=", "op_RightShiftAssignment")]
    [InlineData(">>>=", "op_UnsignedRightShiftAssignment")]
    [InlineData("+=", "op_AdditionAssignment")]
    [InlineData("-=", "op_SubtractionAssignment")]
    [InlineData("*=", "op_MultiplicationAssignment")]
    [InlineData("/=", "op_DivisionAssignment")]
    [InlineData("&=", "op_BitwiseAndAssignment")]
    [InlineData("|=", "op_BitwiseOrAssignment")]
    [InlineData("^=", "op_ExclusiveOrAssignment")]
    public void CompoundOperator_MutatesExistingInstanceAndEmitsInstanceMethod(string token, string metadataName)
    {
        var source = $$"""
            class Counter {
                public var Value: int = 0
                func {{token}}(amount: int) {
                    Value = Value + amount
                }
            }
            class Runner {
                static func Run() -> int {
                    var counter = Counter()
                    let alias = counter
                    counter {{token}} 42
                    return alias.Value
                }
            }
            """;
        Run(source, 42, assembly =>
        {
            var method = assembly.GetType("Counter")!.GetMethod(metadataName)!;
            Assert.False(method.IsStatic);
            Assert.True(method.IsSpecialName);
            Assert.Equal(typeof(void), method.ReturnType);
            Assert.Single(method.GetParameters());
        });
    }

    [Theory]
    [InlineData("class")]
    [InlineData("struct")]
    public void CompoundOperator_ArrayElementEvaluatedOnce(string kind)
    {
        Run($$"""
            {{kind}} Counter {
                public var Value: int = 0
                func +=(amount: int) {
                    Value = Value + amount
                }
            }
            class Runner {
                static var Calls: int = 0
                static func Index() -> int {
                    Calls = Calls + 1
                    return 0
                }
                static func Run() -> int {
                    let counters: Counter[] = [Counter()]
                    counters[Index()] += 41
                    return counters[0].Value + Calls
                }
            }
            """, 42);
    }

    [Fact]
    public void CompoundOperator_PrecedesBinaryFallback()
    {
        Run("""
            class Counter {
                public var Value: int = 0
                func +=(amount: int) {
                    Value = Value + amount
                }
                static func +(left: Counter, right: int) -> Counter {
                    return Counter()
                }
            }
            class Runner {
                static func Run() -> int {
                    var counter = Counter()
                    counter += 42
                    return counter.Value
                }
            }
            """, 42);
    }

    [Fact]
    public void CompoundOperator_StructLocalMutatesStorage()
    {
        Run("""
            struct Counter {
                public var Value: int = 0
                func +=(amount: int) {
                    Value = Value + amount
                }
            }
            class Runner {
                static func Run() -> int {
                    var counter = Counter()
                    counter += 42
                    return counter.Value
                }
            }
            """, 42);
    }

    [Theory]
    [InlineData("static func +=(amount: int) {}", "RAV0332")]
    [InlineData("func +=(amount: int) -> int => amount", "RAV0366")]
    [InlineData("func +=() {}", "RAV0323")]
    [InlineData("func +=(a: int, b: int) {}", "RAV0323")]
    public void InvalidDeclaration_ReportsDiagnostic(string declaration, string diagnostic)
    {
        var compilation = CreateCompilation($"class Counter {{ {declaration} }}");
        Assert.Contains(compilation.GetDiagnostics(), item => item.Id == diagnostic);
    }

    [Fact]
    public void CompoundOperator_ImmutableLocalIsRejected()
    {
        var compilation = CreateCompilation("""
            class Counter {
                func +=(amount: int) {}
            }
            class Runner {
                static func Run() {
                    let counter = Counter()
                    counter += 1
                }
            }
            """);
        Assert.Contains(compilation.GetDiagnostics(), item => item.Id == "RAV0027");
    }

    [Fact]
    public void CompoundOperator_InapplicableCandidateUsesBinaryFallback()
    {
        Run("""
            class Counter {
                public var Value: int = 42
                func +=(amount: int) { Value = 0 }
                static func +(left: Counter, right: Counter) -> Counter => right
            }
            class Runner {
                static func Run() -> int {
                    var counter = Counter()
                    counter += Counter()
                    return counter.Value
                }
            }
            """, 42);
    }

    [Fact]
    public void CompoundOperator_PropertyUsesBinaryOperatorAndSetter()
    {
        Run("""
            class Counter {
                public var Value: int = 42
                func +=(other: Counter) { Value = 0 }
                static func +(left: Counter, right: Counter) -> Counter => right
            }
            class Holder {
                public var Item: Counter { get; set; } = Counter()
            }
            class Runner {
                static func Run() -> int {
                    let holder = Holder()
                    holder.Item += Counter()
                    return holder.Item.Value
                }
            }
            """, 42);
    }

    [Fact]
    public void CompoundOperator_FieldReceiverEvaluatedOnce()
    {
        Run("""
            struct Counter {
                public var Value: int = 0
                func +=(amount: int) { Value = Value + amount }
            }
            class Holder {
                public field Item: Counter = Counter()
            }
            class Runner {
                static var Calls: int = 0
                static val Storage: Holder = Holder()
                static func GetHolder() -> Holder {
                    Calls = Calls + 1
                    return Storage
                }
                static func Run() -> int {
                    GetHolder().Item += 41
                    return Storage.Item.Value + Calls
                }
            }
            """, 42);
    }

    [Fact]
    public void CompoundOperator_AmbiguityDoesNotFallBack()
    {
        var compilation = CreateCompilation("""
            import System.*
            class Counter {
                func +=(value: IComparable) {}
                func +=(value: ICloneable) {}
                static func +(left: Counter, right: string) -> Counter => left
            }
            class Runner {
                static func Run() {
                    var counter = Counter()
                    counter += "hello"
                }
            }
            """);
        Assert.Contains(compilation.GetDiagnostics(), item => item.Id == "RAV0121");
    }

    [Fact]
    public void CompoundOperator_ImportedDeclarationIsCallable()
    {
        var library = CreateCompilation("""
            public class Counter {
                public var Value: int = 0
                func +=(amount: int) { Value = Value + amount }
            }
            """);
        using var libraryStream = new MemoryStream();
        var libraryResult = library.Emit(libraryStream);
        Assert.True(libraryResult.Success, string.Join(Environment.NewLine, libraryResult.Diagnostics));
        var reference = MetadataReference.CreateFromImage(libraryStream.ToArray());
        MetadataReference[] references = [.. TestMetadataReferences.Default, reference];
        var consumer = Compilation.Create("Consumer", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references)
            .AddSyntaxTrees(SyntaxTree.ParseText("""
                class Runner {
                    static func Run() -> int {
                        var counter = Counter()
                        counter += 42
                        return counter.Value
                    }
                }
                """));
        using var stream = new MemoryStream();
        var result = consumer.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("var value: byte = 255; value <<= 1; return value", 254)]
    [InlineData("var value: short = -84; value >>= 1; return value", -42)]
    [InlineData("var value = 85; value %= 43; return value", 42)]
    [InlineData("var value = 21; value <<= 1; return value", 42)]
    [InlineData("var value = -84; value >>= 1; return value", -42)]
    [InlineData("var value = -1; value >>>= 1; return value", 2147483647)]
    [InlineData("return -1 >>> 32", -1)]
    [InlineData("return -1 >>> -1", 1)]
    [InlineData("var value: long = -1; value >>>= 63; return (int)value", 1)]
    [InlineData("var value: long = 21; value <<= 1; return (int)value", 42)]
    public void PredefinedRemainderAndShifts(string body, int expected)
    {
        Run($"class Runner {{ static func Run() -> int {{ {body} }} }}", expected);
    }

    [Fact]
    public void UnsignedShift_BinaryOverloadFallback()
    {
        Run("""
            class Counter {
                public var Value: int = 42
                static func >>>(left: Counter, count: int) -> Counter => left
            }
            class Runner {
                static func Run() -> int {
                    var counter = Counter()
                    counter >>>= 1
                    return counter.Value
                }
            }
            """, 42, assembly => Assert.True(assembly.GetType("Counter")!
                .GetMethod("op_UnsignedRightShift")!.IsSpecialName));
    }

    [Fact]
    public void UnsignedShift_ExposesSemanticOperation()
    {
        var compilation = CreateCompilation("class Runner { static func Run() -> int => -1 >>> 1 }");
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<IBinaryOperation>(compilation.GetSemanticModel(tree).GetOperation(syntax));
        Assert.Equal(Raven.CodeAnalysis.Operations.BinaryOperatorKind.UnsignedRightShift, operation.OperatorKind);
    }

    [Theory]
    [InlineData("class Box<T> {}\nclass Holder { var item: Box<Box<Box<int>>> }")]
    [InlineData("class Box<T> {}\nclass Holder { var item: Box<Box<Box<int>>>=null }")]
    public void NestedGenericClosersStillParse(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        Assert.Empty(tree.GetDiagnostics());
    }

    private static Compilation CreateCompilation(string source) =>
        Compilation.Create("CompoundOperators", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source))
            .AddReferences(TestMetadataReferences.Default);

    private static void Run(string source, int expected, Action<Assembly>? inspect = null)
    {
        var compilation = CreateCompilation(source);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, TestMetadataReferences.Default);
        inspect?.Invoke(loaded.Assembly);
        var run = loaded.Assembly.GetType("Runner")!.GetMethod("Run")!;
        Assert.Equal(expected, run.Invoke(null, null));
    }
}
