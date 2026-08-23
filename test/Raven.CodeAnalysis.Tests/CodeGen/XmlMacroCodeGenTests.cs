using System.Reflection;
using System.Xml.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class XmlMacroCodeGenTests
{
    [Fact]
    public void XmlMacro_ConstructsElementAndEscapesSplices()
    {
        var result = InvokeRun("""
            import Raven.Macros.*

            class Harness {
                public static func Run() -> System.Xml.Linq.XElement {
                    let id = 42
                    let name = "Ada & Bob"
                    let child = System.Xml.Linq.XElement.Parse("<status>ready</status>")
                    return xml! {
                        <user id="$id">
                            <name>$name</name>
                            $child
                        </user>
                    }
                }
            }
            """);

        var value = Assert.IsType<XElement>(result);
        Assert.Equal("user", value.Name.LocalName);
        Assert.Equal("42", value.Attribute("id")!.Value);
        Assert.Equal("Ada & Bob", value.Element("name")!.Value);
        Assert.Equal("ready", value.Element("status")!.Value);
        Assert.DoesNotContain(value.Nodes().OfType<XText>(), static text =>
            string.IsNullOrWhiteSpace(text.Value));
        Assert.Contains("Ada &amp; Bob", value.ToString(SaveOptions.DisableFormatting), StringComparison.Ordinal);
    }

    [Fact]
    public void XmlMacro_ReportsMismatchedClosingElement()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            let value = xml! {
                <user><name>Raven</user>
            }
            """);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(static diagnostic =>
            diagnostic.GetMessage().Contains("XML001", StringComparison.Ordinal)));

        Assert.Same(syntaxTree, diagnostic.Location.SourceTree);
        Assert.Contains("does not match", diagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void XmlMacro_RejectsInvalidEntitiesAtCompileTime()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            let value = xml! {
                <message>invalid &entity;</message>
            }
            """);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(static diagnostic =>
            diagnostic.GetMessage().Contains("XML001", StringComparison.Ordinal)));

        Assert.Contains("Invalid XML literal", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Same(syntaxTree, diagnostic.Location.SourceTree);
    }

    [Fact]
    public void XmlMacro_RejectsUnterminatedElementAtCompileTime()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            let value = xml! {
                <message>unterminated
            }
            """);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(static diagnostic =>
            diagnostic.GetMessage().Contains("XML001", StringComparison.Ordinal)));

        Assert.Contains("closing tag", diagnostic.GetMessage(), StringComparison.OrdinalIgnoreCase);
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
