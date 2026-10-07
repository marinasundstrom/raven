using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class SealedHierarchyCaseCodeGenTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GenericCaseMemberCallsAndRecordFormattingUseEmittedArity(bool release)
    {
        var compilation = Compilation.Create("GenericCaseMembers", [SyntaxTree.ParseText("""
            public sealed interface Box<T> {
                func Read() -> T
                public record Item<T>(Value: T) : Box<T> {
                    func Read() -> T => Value
                }
            }
            public class Consumer {
                static func Read() -> int => Box.Item<int>(42).Read()
                static func Format() -> string => Box.Item<int>(42).ToString()
            }
            """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithOptimizationLevel(release ? OptimizationLevel.Release : OptimizationLevel.Debug));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        var consumer = loaded.Assembly.GetType("Consumer")!;
        Assert.Equal(42, consumer.GetMethod("Read")!.Invoke(null, null));
        Assert.Contains("42", (string)consumer.GetMethod("Format")!.Invoke(null, null)!);
    }
}
