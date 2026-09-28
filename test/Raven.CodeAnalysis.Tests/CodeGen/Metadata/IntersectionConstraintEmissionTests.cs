using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests.Utilities;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class IntersectionConstraintEmissionTests
{
    [Theory]
    [InlineData("Base & IExtra")]
    [InlineData("IExtra & Base")]
    public void ClassAndInterfaceBounds_EmitOrdinaryConstraintsAndExecute(string bounds)
    {
        var tree = SyntaxTree.ParseText($$"""
            public interface IExtra { func Extra() -> int }
            public open class Base { public func Read() -> int => 40 }
            public class Derived: Base, IExtra { public func Extra() -> int => 2 }
            public class Runner {
                public static func Sum<T>(value: T) -> int where T: {{bounds}} {
                    return value.Read() + value.Extra()
                }
                public static func Run() -> int => Sum(Derived())
            }
            """);
        var compilation = Compilation.Create("intersection-constraints", [tree],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        var consumer = Compilation.Create("intersection-consumer",
            [SyntaxTree.ParseText("func Run() -> int => Runner.Sum(Derived())")],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromImage(stream.ToArray())],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(consumer.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

        var invalidConsumer = Compilation.Create("invalid-intersection-consumer",
            [SyntaxTree.ParseText("func Run() -> int => Runner.Sum(Base())")],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromImage(stream.ToArray())],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(invalidConsumer.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);

        using var loaded = TestAssemblyLoader.LoadFromStream(stream, TestMetadataReferences.Default);
        var runner = loaded.Assembly.GetType("Runner", throwOnError: true)!;
        var parameter = Assert.Single(runner.GetMethod("Sum")!.GetGenericArguments());
        Assert.Equal(new[] { "Base", "IExtra" }, parameter.GetGenericParameterConstraints().Select(t => t.Name).OrderBy(n => n));
        Assert.Equal(42, runner.GetMethod("Run")!.Invoke(null, null));
        Assert.Throws<ArgumentException>(() => runner.GetMethod("Sum")!.MakeGenericMethod(loaded.Assembly.GetType("Base")!));
    }
}
