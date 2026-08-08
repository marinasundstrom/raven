using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class QueryMacroToolingAcceptanceTests
{
    [Fact]
    public void CheckedInQueryMacro_ReportsEachEmbeddedRavenExpression()
    {
        var macroReference = CreateCheckedInQueryMacroReference();
        const string source = """
            let result = query! {
                from value in values
                where value > minimum
                select value * scale
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "query-fragments.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var regions = compilation.GetSemanticModel(syntaxTree)
            .GetMacroInputSnapshot(invocation)
            .FragmentRegions;

        Assert.Equal(
            ["values", "value > minimum", "value * scale"],
            regions.Select(region => source.Substring(region.Span.Start, region.Span.Length).Trim()));
        Assert.All(regions, static region => Assert.Equal(MacroFragmentKind.Expression, region.Kind));
        Assert.Empty(regions[0].Locals);
        Assert.Equal("value", Assert.Single(regions[1].Locals).Name);
        Assert.Equal("value", Assert.Single(regions[2].Locals).Name);
    }

    [Fact]
    public void CheckedInQueryMacro_RoutesCallerCompletionInsideSourceExpression()
    {
        var macroReference = CreateCheckedInQueryMacroReference();
        const string source = """
            class QueryHost {
                func Test() {
                    let sourceText = "hello"
                    let result = query! {
                        from value in sourceText.
                        select value
                    }
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "query-completion.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var position = source.IndexOf("sourceText.", StringComparison.Ordinal) + "sourceText.".Length;

        var items = compilation.GetSemanticModel(syntaxTree)
            .GetCompletions(position)
            .ToArray();

        Assert.Contains(items, static item => item.DisplayText == "Length");
    }

    [Fact]
    public void CheckedInQueryMacro_CompletesMembersOfIntroducedRangeVariable()
    {
        var macroReference = CreateCheckedInQueryMacroReference();
        const string source = """
            class Customer(val Name: string)

            class QueryHost {
                val Customers: Customer[] = []

                func Test() {
                    let result = query! {
                        from customer in Customers
                        where customer.Name.Length > 0
                        select customer.
                    }
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "query-range-completion.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var position = source.LastIndexOf("customer.", StringComparison.Ordinal) + "customer.".Length;
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var region = compilation.GetSemanticModel(syntaxTree)
            .GetMacroInputSnapshot(invocation)
            .FindFragmentRegion(position);

        var local = Assert.Single(region!.Locals);
        Assert.Equal("customer", local.Name);
        Assert.Equal("Customer", local.Type.Name);

        var items = compilation.GetSemanticModel(syntaxTree)
            .GetCompletions(position)
            .ToArray();

        Assert.Contains(items, static item => item.DisplayText == "Name");
    }

    private static Compilation CreateConsumerCompilation(
        SyntaxTree tree,
        MacroReference macroReference)
        => Compilation.Create(
                $"QueryMacroConsumer_{Guid.NewGuid():N}",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(macroReference);

    private static MacroReference CreateCheckedInQueryMacroReference()
    {
        var repositoryRoot = Path.GetFullPath(
            Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        var sourcePath = Path.Combine(
            repositoryRoot,
            "samples",
            "projects",
            "macro-freestanding",
            "macros",
            "FreestandingMacros.rvn");
        var macroTree = SyntaxTree.ParseText(File.ReadAllText(sourcePath), path: sourcePath);
        var codeAnalysisReference = MetadataReference.CreateFromFile(
            typeof(IMacroDefinition).Assembly.Location);
        var macroCompilation = Compilation.Create(
                $"CheckedInQueryMacro_{Guid.NewGuid():N}",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(macroTree)
            .AddReferences([
                .. TestMetadataReferences.DefaultWithRavenMacros,
                codeAnalysisReference,
            ])
            .AddMacroReferences(MacroReference.CreateFromFile(
                ((PortableExecutableReference)TestMetadataReferences.RavenMacros).FilePath!));

        using var image = new MemoryStream();
        var emitResult = macroCompilation.Emit(image);
        Assert.True(
            emitResult.Success,
            string.Join(Environment.NewLine, emitResult.Diagnostics));

        return MacroReference.CreateFromImage(
            image.ToArray(),
            display: "checked-in query macro sample");
    }
}
