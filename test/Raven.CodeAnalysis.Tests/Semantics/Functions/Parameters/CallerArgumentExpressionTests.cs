using System.Reflection;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax;
namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class CallerArgumentExpressionTests
{
    [Fact]
    public void CSharpNamedParamsOracle()
    {
        Assert.Equal("fallback", CallerCaptureFixture.Params());
        Assert.Equal("[1, 2]", CallerCaptureFixture.Params(values: [1, 2]));
        Assert.Equal("CallerCaptureFixture.Params(values: 1)", CallerCaptureFixture.Params(values: 1));
    }

    [Theory]
    [InlineData("Capture(1 + 2)", "1 + 2")]
    [InlineData("Capture(value: 1 + 2)", "1 + 2")]
    [InlineData("Capture(((1 + 2)))", "1 + 2")]
    [InlineData("Capture(1 /* retained */ + 2)", "1 /* retained */ + 2")]
    [InlineData("Capture(1, text: \"explicit\")", "explicit")]
    [InlineData("Capture()", "fallback")]
    public void SourceAttributeCapturesOnlyOmittedArgument(string call, string expected)
    {
        var source = $$"""
            import System.Runtime.CompilerServices.*
            public class Probe {
                static func Capture(value: int = 0, [CallerArgumentExpression("value")] text: string = "fallback") -> string => text
                static func Run() -> string => {{call}}
            }
            """;
        Assert.Equal(expected, Run(source));
    }

    [Fact]
    public void FrameworkGuardCapturesParameterName()
    {
        var source = """
            import System.*
            public class Probe {
                static func Run() -> string {
                    let namespaces: object? = null
                    try { ArgumentNullException.ThrowIfNull(namespaces) }
                    catch (ArgumentNullException error) { return error.ParamName! }
                    return "no exception"
                }
            }
            """;
        Assert.Equal("namespaces", Run(source));
    }

    [Theory]
    [InlineData("missing")]
    [InlineData("text")]
    public void InvalidSiblingUsesDeclaredDefault(string sibling)
    {
        Assert.Equal("fallback", Run($$"""
            import System.Runtime.CompilerServices.*
            public class Probe {
                static func Capture(value: int, [CallerArgumentExpression("{{sibling}}")] text: string = "fallback") -> string => text
                static func Run() -> string => Capture(42)
            }
            """));
    }

    [Theory]
    [InlineData("Capture(1 + 2)", "1 + 2")]
    [InlineData("Capture(text: \"custom\", value: 2)", "custom")]
    [InlineData("Generic(1 + 2)", "1 + 2")]
    [InlineData("(1 + 2).Describe()", "1 + 2")]
    [InlineData("Describe(1 + 2)", "1 + 2")]
    [InlineData("1 + 2 |> Describe()", "1 + 2")]
    [InlineData("CaptureObject(42)", "42")]
    [InlineData("Captured(1 + 2).Text", "1 + 2")]
    [InlineData("Params(values: 1)", "Params(values: 1)")]
    [InlineData("Params()", "fallback")]
    [InlineData("Params(values: [1, 2])", "[1, 2]")]
    public void ImportedAttributeWorksAcrossCallForms(string call, string expected)
    {
        Assert.Equal(expected, Run($$"""
            import Raven.CodeAnalysis.Tests.*
            import Raven.CodeAnalysis.Tests.CallerCaptureFixture.*
            public class Probe {
                static func Run() -> string => {{call}}
            }
            """));
    }

    private static object? Run(string source)
    {
        var compilation = Compilation.Create("CallerProbe", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(CallerCaptureFixture).Assembly.Location)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        return loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null);
    }
}

public static class CallerCaptureFixture
{
    public static string Params([CallerArgumentExpression("values")] string text = "fallback", params int[] values) => text;
    public static string Capture(int value, [CallerArgumentExpression("value")] string text = "fallback") => text;
    public static string Generic<T>(T value, [CallerArgumentExpression("value")] string text = "fallback") => text;
    public static string Describe(this int value, [CallerArgumentExpression("value")] string text = "fallback") => text;
    public static string CaptureObject(int value, [CallerArgumentExpression("value")] object text = null!) => (string)text;
}
public sealed class Captured
{
    public Captured(int value, [CallerArgumentExpression("value")] string text = "fallback") { Text = text; }
    public string Text { get; }
}

public sealed class CallerArgumentExpressionSemanticTests
{
    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_OmittedArgumentCapturesSourceText()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Runtime.CompilerServices.*
            class Probe {
                static func Capture(value: int, [CallerArgumentExpression("value")] text: string = "fallback") -> string => text
                static func Run() -> string => Capture(1 + 2)
            }
            """);
        var compilation = Compilation.Create("CallerSemantic", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        var literal = Assert.IsType<BoundLiteralExpression>(bound.Arguments.Last());
        Assert.Equal("1 + 2", literal.Value);
    }
}
