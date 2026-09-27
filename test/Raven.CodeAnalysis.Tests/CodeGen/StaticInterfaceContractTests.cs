using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class StaticInterfaceContractTests
{
    [Theory]
    [InlineData("")]
    [InlineData("abstract ")]
    public void AuthoredStaticContractsSupportConstrainedCalls(string abstractModifier)
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("StaticNumericContract", [SyntaxTree.ParseText("""
            public interface Number<T> {
                static ABSTRACTval Zero: T { get; }
                static ABSTRACTfunc Add(left: T, right: T) -> T
                static func +(left: T, right: T) -> T
                static func +(value: T) -> T
            }
            public struct Count : Number<Count> {
                public var Value: int
                static val Zero: Count => Count()
                static func Add(left: Count, right: Count) -> Count {
                    var result = Count()
                    result.Value = left.Value + right.Value
                    return result
                }
                static func +(left: Count, right: Count) -> Count => Add(left, right)
                static func +(value: Count) -> Count => value
            }
            public class Consumer {
                static func Sum<T>(left: T, right: T) -> T where T: Number<T> {
                    return +T.Add(left, right) + T.Zero
                }
                static func Run() -> int {
                    var value = Count()
                    value.Value = 21
                    return Sum<Count>(value, value).Value
                }
            }
            """.Replace("ABSTRACT", abstractModifier))], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)).AddReferences(references);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(new MemoryStream(output.ToArray()), references);
        Assert.Equal(42, loaded.Assembly.GetType("Consumer", true)!.GetMethod("Run")!.Invoke(null, null));
        var contract = loaded.Assembly.GetType("Number`1", true)!;
        foreach (var method in contract.GetMethods()) {
            Assert.True(method.IsStatic);
            Assert.True(method.IsAbstract);
            Assert.True(method.IsVirtual);
        }
    }
    [Theory]
    [InlineData("static func Create() -> int", "func Create() -> int => 1")]
    [InlineData("static val Zero: int { get; }", "val Zero: int => 0")]
    [InlineData("static func Create() -> int", "static func Create() -> string => \"wrong\"")]
    public void StaticContractsRejectMismatchedImplementations(string member, string implementation)
    {
        var source = "interface Contract { " + member + " } class Consumer : Contract { " + implementation + " }";
        var compilation = Compilation.Create("InvalidStaticContract", [SyntaxTree.ParseText(source)],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)).AddReferences(TestMetadataReferences.Default);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

}
