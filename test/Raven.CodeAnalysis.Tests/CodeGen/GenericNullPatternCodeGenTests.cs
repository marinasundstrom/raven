using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class GenericNullPatternCodeGenTests
{
    [Fact]
    public void GenericNullPatternsHandleReferencesValuesNullableValuesAndReferenceBounds()
    {
        const string source = """
            open class Node { }
            class Probe {
                static func IsNull<T>(value: T) -> bool => value is null
                static func IsNotNull<T>(value: T) -> bool => value is not null
                static func BoundIsNull<T>(value: T) -> bool where T: Node? => value is null
                static func Run() -> int {
                    if !IsNull<string?>(null) || IsNotNull<string?>(null) { throw System.Exception("null reference") }
                    if IsNull<string>("value") || !IsNotNull<string>("value") { throw System.Exception("reference") }
                    if IsNull<int>(0) || !IsNotNull<int>(0) { throw System.Exception("value") }
                    if !IsNull<int?>(null) || IsNotNull<int?>(null) { throw System.Exception("nullable empty") }
                    if IsNull<int?>(42) || !IsNotNull<int?>(42) { throw System.Exception("nullable value") }
                    if !BoundIsNull<Node?>(null) || BoundIsNull<Node>(Node()) { throw System.Exception("reference bound") }
                    return 42
                }
            }
            """;
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("GenericNullPattern", [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        Assert.Equal(42, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
