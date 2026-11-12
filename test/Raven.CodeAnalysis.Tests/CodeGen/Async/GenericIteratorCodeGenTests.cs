using System.Collections;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen.Async;

[Trait("Target", "DotNet")]
public sealed class GenericIteratorCodeGenTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GenericMethodIteratorPreservesTypeAndSupportsRepeatedEnumeration(bool referenceType)
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            public class Probe {
                static func Values<T>(value: T) -> IEnumerable<T> {
                    yield value
                    yield value
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("GenericIterator", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Values")!.MakeGenericMethod(referenceType ? typeof(string) : typeof(int));
        object value = referenceType ? "value" : 42;
        var values = (IEnumerable)method.Invoke(null, [value])!;
        Assert.Equal<object>([value, value], values.Cast<object>());
        Assert.Equal<object>([value, value], values.Cast<object>());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GenericForeachIteratorMapsEnumeratorAndHoistedLocalTypes(bool referenceType)
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            public class Probe {
                static func Values<T>(values: IEnumerable<T>) -> IEnumerable<T> {
                    for value in values {
                        yield value
                    }
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("GenericIteratorLoop", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Values")!.MakeGenericMethod(referenceType ? typeof(string) : typeof(int));
        object input = referenceType ? new[] { "first", "second" } : new[] { 1, 2 };
        var values = (IEnumerable)method.Invoke(null, [input])!;
        Assert.Equal(((IEnumerable)input).Cast<object>(), values.Cast<object>());
    }

    [Fact]
    public void GenericContainingTypeAndMethodHaveIndependentParameters()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            public class Probe<T> {
                func Values<U>(value: T, other: U) -> IEnumerable<T> {
                    yield value
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("GenericIteratorOwner", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var type = loaded.Assembly.GetType("Probe`1")!.MakeGenericType(typeof(string));
        var method = type.GetMethod("Values")!.MakeGenericMethod(typeof(int));
        var values = (IEnumerable)method.Invoke(Activator.CreateInstance(type), ["value", 42])!;
        Assert.Equal<object>(["value"], values.Cast<object>());
    }

    [Fact]
    public void GenericIteratorStatePreservesInterfaceConstraints()
    {
        var tree = SyntaxTree.ParseText("""
            import System.*
            import System.Collections.Generic.*
            public class Probe {
                static func Values<T>(value: T) -> IEnumerable<T> where T: IDisposable {
                    yield value
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("GenericIteratorConstraints", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Values")!.MakeGenericMethod(typeof(MemoryStream));
        using var value = new MemoryStream();
        var values = (IEnumerable)method.Invoke(null, [value])!;
        Assert.Same(value, Assert.Single(values.Cast<object>()));
        var stateParameter = Assert.Single(values.GetType().GetGenericTypeDefinition().GetGenericArguments());
        Assert.Null(stateParameter.DeclaringMethod);
        Assert.Contains(stateParameter.GetGenericParameterConstraints(), constraint => constraint.FullName == "System.IDisposable");
    }

    [Fact]
    public void GenericEnumeratorReturnUsesConstructedState()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            public class Probe {
                static func Values<T>(value: T) -> IEnumerator<T> {
                    yield value
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("GenericEnumerator", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Values")!.MakeGenericMethod(typeof(int));
        using var iterator = (IEnumerator<int>)method.Invoke(null, [42])!;
        Assert.True(iterator.MoveNext());
        Assert.Equal(42, iterator.Current);
        Assert.False(iterator.MoveNext());
    }

    [Fact]
    public async Task GenericAsyncIteratorMapsNestedStateTypes()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            import System.Threading.Tasks.*
            public class Probe {
                static async func Values<T>(value: T) -> IAsyncEnumerable<T> {
                    await Task.CompletedTask
                    yield value
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("GenericAsyncIterator", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Values")!.MakeGenericMethod(typeof(int));
        var values = (IAsyncEnumerable<int>)method.Invoke(null, [42])!;
        var result = new List<int>();
        await foreach (var value in values)
            result.Add(value);
        Assert.Equal([42], result);
    }
}
