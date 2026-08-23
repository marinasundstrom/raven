using System.Reflection;
using System.Text.Json.Nodes;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class JsonMacroCodeGenTests
{
    [Fact]
    public void JsonMacro_ConstructsObjectAndEvaluatesSplices()
    {
        var result = InvokeRun("""
            import Raven.Macros.*

            class Harness {
                public static func Run() -> System.Text.Json.Nodes.JsonObject {
                    let name = "Ada"
                    let age = 42
                    return json! {
                        "name": "$name",
                        "age": $age,
                        "nested": { "active": true },
                        "values": [1, ${age + 1}]
                    }
                }
            }
            """);

        var value = Assert.IsType<JsonObject>(result);
        Assert.Equal("Ada", value["name"]!.GetValue<string>());
        Assert.Equal(42, value["age"]!.GetValue<int>());
        Assert.True(value["nested"]!["active"]!.GetValue<bool>());
        Assert.Equal(43, value["values"]![1]!.GetValue<int>());
    }

    [Fact]
    public void JsonMacro_ReportsMalformedObjectAtAuthoredBody()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            let value = json! {
                "name" "Ada"
            }
            """);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(static diagnostic =>
            diagnostic.GetMessage().Contains("JSON001", StringComparison.Ordinal)));

        Assert.Same(syntaxTree, diagnostic.Location.SourceTree);
        Assert.Contains("Expected ':'", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void JsonMacro_RejectsInvalidJsonEscapesAtCompileTime()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            let value = json! {
                "path": "invalid\qescape"
            }
            """);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(static diagnostic =>
            diagnostic.GetMessage().Contains("JSON001", StringComparison.Ordinal)));

        Assert.Contains("Invalid JSON literal", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Same(syntaxTree, diagnostic.Location.SourceTree);
    }

    [Fact]
    public void JsonMacro_RejectsUnterminatedStringAtCompileTime()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            let value = json! {
                "name": "unterminated
            }
            """);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(static diagnostic =>
            diagnostic.GetMessage().Contains("JSON001", StringComparison.Ordinal)));

        Assert.Contains("Unterminated JSON string", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Same(syntaxTree, diagnostic.Location.SourceTree);
    }

    private static object? InvokeRun(string source)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(
            result.Success,
            string.Join(Environment.NewLine, result.Diagnostics) + Environment.NewLine +
            compilation.GetSemanticModel(syntaxTree).GetExpandedRoot().ToFullString());

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        return method!.Invoke(null, null);
    }

    private static Compilation CreateCompilation(SyntaxTree syntaxTree)
        => Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros);
}
