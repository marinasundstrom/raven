using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class RefFieldCodeGenTests
{
    [Fact]
    public void RefField_CanReferenceAndMutateLocalStorage()
    {
        const string code = """
            ref struct IntReference {
                field Value: &int
            }

            class RefFieldRuntime {
                static func Run() -> int {
                    var value = 41
                    var reference = IntReference()
                    reference.Value = &value
                    *reference.Value = 42
                    value
                }
            }
            """;

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create(
                "ref_field_codegen",
                [syntaxTree],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("RefFieldRuntime", throwOnError: true)!;
        var method = type.GetMethod("Run", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;

        Assert.Equal(42, (int)method.Invoke(null, null)!);
    }
}
