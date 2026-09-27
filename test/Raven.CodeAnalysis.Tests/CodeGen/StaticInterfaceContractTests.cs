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
            public interface Identity<T> {
                static ABSTRACTval Zero: T { get; }
            }
            public interface Ordered<T> {
                func CompareTo(other: T) -> int
            }
            public interface Number<T> : Identity<T>, Ordered<T> {
                static ABSTRACTfunc Add(left: T, right: T) -> T
                static func +(left: T, right: T) -> T
                static func +(value: T) -> T
            }
            public struct Count : Number<Count> {
                public var Value: int
                static val Zero: Count => Count()
                func CompareTo(other: Count) -> int => Value - other.Value
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
                    let result = +T.Add(left, right) + T.Zero
                    if result.CompareTo(T.Zero) < 0 {
                        return T.Zero
                    }
                    return result
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

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ImportedInterfaceConstraintEmitsInTargetMetadataMode(bool targetMetadata)
    {
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        if (targetMetadata)
            options = options.WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
                .WithTargetCoreAssemblyName("System.Runtime");
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ImportedConstraint", [SyntaxTree.ParseText("""
            import System.*
            public class Consumer {
                static func Compare<T>(left: T, right: T) -> int where T: IComparable<T>
                    => left.CompareTo(right)
                static func Run() -> int => Compare<int>(1, 2)
            }
            """)], references, options);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(new MemoryStream(output.ToArray()), references);
        Assert.Equal(-1, loaded.Assembly.GetType("Consumer", true)!.GetMethod("Run")!.Invoke(null, null));
    }

}
