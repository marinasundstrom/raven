using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class ArrayAllocationTests
{
    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("int[](3)")]
    [InlineData("Token[](count)")]
    [InlineData("System.String[](3)")]
    [InlineData("T[][](3)")]
    [InlineData("object?[](3)")]
    public void AllocationParsesAsArrayTypeInvocation(string expression)
    {
        var tree = SyntaxTree.ParseText($"let values = {expression}");
        Assert.Empty(tree.GetDiagnostics());
        var invocation = Assert.Single(tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>());
        Assert.IsType<ArrayTypeSyntax>(invocation.Expression);
        Assert.Single(invocation.ArgumentList.Arguments);
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void AllocationEvaluatesCountOnceAndDefaultInitializesElements()
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                private static var calls: int = 0
                private static func Count() -> int { calls++; return 3 }
                static func Run() -> int {
                    val values = int[](Count())
                    val references = object?[](count: 2)
                    if values.Length != 3 || values[0] != 0 || references[0] is not null { return -1 }
                    values[1] = 40
                    return values[1] + calls + references.Length
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ArrayAllocation", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(43, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, null));
    }

    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("count")]
    [InlineData("3")]
    public void AllocationExposesLengthOperationAndPortableBody(string count)
    {
        var tree = SyntaxTree.ParseText($"class Probe {{ static func Make(count: int) -> int[] => int[]({count}) }}");
        var compilation = Compilation.Create("AllocationContract", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<Raven.CodeAnalysis.Operations.IArrayCreationOperation>(model.GetOperation(invocation));
        Assert.Equal(SpecialType.System_Int32, operation.Length.Type!.SpecialType);
        Assert.IsAssignableFrom<ArrayTypeSymbol>(model.GetTypeInfo(invocation).Type);
        var methodSyntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (MethodSymbol)model.GetDeclaredSymbol(methodSyntax)!;
        var capabilities = Raven.CodeAnalysis.CodeGen.Portable.ReflectionEmitCapabilities.Shared;
        Assert.True(Raven.CodeAnalysis.CodeGen.Portable.SourceCallablePlan.TryCreate(method, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
    }

    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(0)]
    [InlineData(3)]
    [InlineData(-1)]
    public void AllocationPreservesRuntimeLengthBehavior(int length)
    {
        var tree = SyntaxTree.ParseText("class Probe { static func Make(count: int) -> int[] => int[](count) }");
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("AllocationLength", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("Probe", true)!.GetMethod("Make")!;
        if (length < 0)
        {
            var exception = Assert.Throws<TargetInvocationException>(() => method.Invoke(null, [length]));
            Assert.IsType<OverflowException>(exception.InnerException);
        }
        else
        {
            var array = Assert.IsType<int[]>(method.Invoke(null, [length]));
            Assert.Equal(length, array.Length);
            Assert.All(array, value => Assert.Equal(0, value));
        }
    }

    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("int[]()")]
    [InlineData("int[](1, 2)")]
    [InlineData("int[](true)")]
    [InlineData("int[](-1)")]
    [InlineData("int[](missing: 2)")]
    public void InvalidAllocationReportsDiagnostic(string expression)
    {
        var tree = SyntaxTree.ParseText($"let values = {expression}");
        var compilation = Compilation.Create("InvalidAllocation", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void GenericAllocationPreservesElementTypeAndDefaultStorage()
    {
        var tree = SyntaxTree.ParseText("class Probe { static func Make<T>(count: int) -> T[] => T[](count) }");
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("GenericAllocation", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("Probe", true)!.GetMethod("Make")!;
        foreach (var type in new[] { typeof(int), typeof(string), typeof(DateTime), typeof(int[]) })
        {
            var array = Assert.IsAssignableFrom<Array>(method.MakeGenericMethod(type).Invoke(null, [2]));
            Assert.Equal(type.MakeArrayType(), array.GetType());
            Assert.Equal(2, array.Length);
            Assert.Equal(type.IsValueType ? Activator.CreateInstance(type) : null, array.GetValue(0));
        }
    }

    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("10", 10)]
    [InlineData("2 + 3", 5)]
    [InlineData("0", 0)]
    [InlineData("count", null)]
    public void AllocationRetainsCompileTimeLength(string count, int? expectedLength)
    {
        var tree = SyntaxTree.ParseText($"class Probe {{ static func Make(count: int) {{ let values = int[]({count}) }} }}");
        var compilation = Compilation.Create("AllocationSize", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var type = Assert.IsAssignableFrom<ArrayTypeSymbol>(compilation.GetSemanticModel(tree).GetTypeInfo(invocation).Type);
        Assert.Equal(expectedLength, type.FixedLength);
    }
}
