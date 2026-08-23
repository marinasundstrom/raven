using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class DataLiteralMacroToolingAcceptanceTests
{
    [Theory]
    [InlineData("json", "json", "name", "age + 1")]
    [InlineData("xml", "xml", "name", "age + 1")]
    public void CheckedInMacro_ProjectsDslAndReportsRavenSplices(
        string macroName,
        string languageId,
        string firstExpression,
        string secondExpression)
    {
        var source = macroName == "json"
            ? """
                import Raven.Macros.*

                func Build() {
                    let name = "Ada"
                    let age = 42
                    let value = json! {
                        "name": "$name",
                        "next": ${age + 1}
                    }
                }
                """
            : """
                import Raven.Macros.*

                func Build() {
                    let name = "Ada"
                    let age = 42
                    let value = xml! {
                        <person name="$name">${age + 1}</person>
                    }
                }
                """;
        var syntaxTree = SyntaxTree.ParseText(source, path: $"{macroName}-tooling.rvn");
        var compilation = CreateCompilation(syntaxTree);
        var invocation = Assert.Single(
            syntaxTree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>());

        var snapshot = compilation.GetMacroInputSnapshot(invocation);
        var projection = compilation.GetMacroEmbeddedLanguageProjection(invocation);

        Assert.Equal(
            [firstExpression, secondExpression],
            snapshot.FragmentRegions.Select(region =>
                source.Substring(region.Span.Start, region.Span.Length)));
        Assert.All(snapshot.FragmentRegions, static region =>
            Assert.Equal(MacroFragmentKind.Expression, region.Kind));
        Assert.NotNull(projection);
        Assert.Equal(languageId, projection.LanguageId);
        Assert.Equal(projection.Span.Length, projection.Text.Length);
        Assert.DoesNotContain("$" + firstExpression, projection.Text, StringComparison.Ordinal);
        Assert.DoesNotContain("${" + secondExpression + "}", projection.Text, StringComparison.Ordinal);
    }

    [Fact]
    public void CheckedInJsonMacro_ResolvesCallerSymbolInsideSplice()
    {
        const string source = """
            import Raven.Macros.*

            func Build() {
                let age = 42
                let value = json! {
                    "age": ${age + 1}
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);
        var invocation = Assert.Single(
            syntaxTree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>());
        var position = source.LastIndexOf("age +", StringComparison.Ordinal) + 1;

        var info = compilation.GetMacroFragmentSemanticInfo(invocation, position);
        var symbol = Assert.IsAssignableFrom<ILocalSymbol>(info?.SymbolInfo.Symbol);

        Assert.Equal("age", symbol.Name);
        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);
    }

    private static Compilation CreateCompilation(SyntaxTree syntaxTree)
        => Compilation.Create(
                "DataLiteralMacroTooling",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddMacroReferences(MacroReference.CreateFromFile(
                ((PortableExecutableReference)TestMetadataReferences.RavenMacros).FilePath!));
}
