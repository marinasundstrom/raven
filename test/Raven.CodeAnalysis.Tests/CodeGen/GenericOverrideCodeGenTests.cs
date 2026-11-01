using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class GenericOverrideCodeGenTests
{
    [Fact]
    public void RenamedTypeParameterInConstructedSignatureDispatchesThroughBase()
    {
        const string source = """
            class Box<T> { val Value: T; init(value: T) { Value = value } }
            open class Base {
                virtual func Accept<T>(visitor: Box<T>) -> T { throw System.Exception("base") }
            }
            class Derived : Base {
                override func Accept<TResult>(visitor: Box<TResult>) -> TResult => visitor.Value
            }
            class Probe {
                static func Run() -> int {
                    let value: Base = Derived()
                    return value.Accept<int>(Box<int>(42)) + Derived().Accept<int>(Box<int>(42))
                }
            }
            """;
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("GenericOverride", [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.True(!compilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, compilation.GetDiagnostics()));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        Assert.Equal(84, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("override func Accept<T, U>(value: T) -> T => value")]
    [InlineData("override func Accept<T>(value: string) -> T { throw System.Exception() }")]
    public void DifferentArityOrParameterTypeDoesNotOverride(string member)
    {
        var source = $$"""
            open class Base { virtual func Accept<T>(value: T) -> T => value }
            class Derived : Base { {{member}} }
            """;
        var compilation = Compilation.Create("InvalidOverride", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0307");
    }
}
