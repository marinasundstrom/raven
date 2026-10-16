using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TruthOperatorTests
{
    private const string Truth = """
        class Truth {
            public var State: int = 0
            static func true(value: Truth) -> bool => value.State > 0
            static func false(value: Truth) -> bool => value.State < 0
            static func &(left: Truth, right: Truth) -> Truth => right
            static func |(left: Truth, right: Truth) -> Truth => right
        }
        """;

    [Theory]
    [InlineData("&&", -1, 0, -1)]
    [InlineData("&&", 0, 1, 42)]
    [InlineData("||", 1, 0, 1)]
    [InlineData("||", 0, 1, 42)]
    [InlineData("and", -1, 0, -1)]
    [InlineData("or", 1, 0, 1)]
    public void ConditionalLogicalOperatorsShortCircuit(string token, int state, int calls, int expected)
    {
        Run(Truth + "\n" + $$"""
            class Runner {
                static var Calls: int = 0
                static func Right() -> Truth {
                    Calls = Calls + 1
                    let result = Truth()
                    result.State = 42
                    return result
                }
                static func Run() -> int {
                    let left = Truth()
                    left.State = {{state}}
                    let result = left {{token}} Right()
                    if Calls != {{calls}} || result.State != {{expected}} { return 1 }
                    return 42
                }
            }
            """);
    }

    [Fact]
    public void TruthOperatorsControlConditionsAndEmitStandardNames()
    {
        Run(Truth + "\n" + """
            class Runner {
                static func Run() -> int {
                    let value = Truth()
                    value.State = 1
                    while value { value.State = 0 }
                    if value { return 1 }
                    value.State = 1
                    if value { return 42 }
                    return 2
                }
            }
            """);
    }

    [Theory]
    [InlineData("static func true(value: Truth) -> bool => true", "RAV0369")]
    [InlineData("static func true(value: Truth) -> int => 1", "RAV0368")]
    [InlineData("static func &&(a: Truth, b: Truth) -> Truth => a", "RAV0370")]
    public void InvalidTruthDeclarationIsRejected(string member, string diagnostic)
    {
        Assert.Contains(Create($"class Truth {{ {member} }}").GetDiagnostics(), d => d.Id == diagnostic);
    }

    [Fact]
    public void ImportedTruthOperatorsShortCircuit()
    {
        using var library = new MemoryStream();
        var emitted = Create(Truth.Replace("class Truth", "public class Truth")).Emit(library);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        MetadataReference[] references = [.. TestMetadataReferences.Default, MetadataReference.CreateFromImage(library.ToArray())];
        var consumer = Compilation.Create("TruthConsumer", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references).AddSyntaxTrees(SyntaxTree.ParseText("""
                class Runner {
                    static func Run() -> int {
                        let value = Truth()
                        value.State = 1
                        let result = value || Truth()
                        if result { return 42 }
                        return 1
                    }
                }
                """));
        using var stream = new MemoryStream();
        var result = consumer.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    private static Compilation Create(string source) => Compilation.Create("TruthOperators",
        new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
        .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);

    private static void Run(string source)
    {
        using var stream = new MemoryStream();
        var result = Create(source).Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, TestMetadataReferences.Default);
        Assert.True(loaded.Assembly.GetType("Truth")!.GetMethod("op_True")!.IsSpecialName);
        Assert.True(loaded.Assembly.GetType("Truth")!.GetMethod("op_False")!.IsSpecialName);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }
}
