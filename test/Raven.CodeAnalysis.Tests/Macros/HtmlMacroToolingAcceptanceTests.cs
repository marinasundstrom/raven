using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class HtmlMacroToolingAcceptanceTests
{
    [Fact]
    public void CheckedInHtmlMacro_ProvidesCompleteToolingSnapshotAndAuthoredDiagnostics()
    {
        var macroReference = CreateCheckedInHtmlMacroReference();
        const string validSource = """
            let view = Html! {
                <button onClick={increment} title="Counter">
                    Count: {count}
                </button>
            }
            """;
        var validTree = SyntaxTree.ParseText(validSource, path: "valid-html.rvn");
        var validCompilation = CreateConsumerCompilation(validTree, macroReference);
        var invocation = validTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var semanticModel = validCompilation.GetSemanticModel(validTree);

        var snapshot = semanticModel.GetMacroInputSnapshot(invocation);

        Assert.Same(snapshot, semanticModel.GetMacroInputSnapshot(invocation));
        Assert.Contains(
            snapshot.Tokens,
            static token =>
                token.Text == "button" &&
                token.KindName == nameof(SyntaxKind.IdentifierToken) &&
                token.Classification == MacroTokenClassification.Identifier);
        Assert.Contains(
            snapshot.Tokens,
            static token =>
                token.Text == "<" &&
                token.KindName == nameof(SyntaxKind.LessThanToken) &&
                token.Classification == MacroTokenClassification.Punctuation);
        Assert.Equal(
            ["increment", "count"],
            snapshot.FragmentRegions
                .Select(region => validSource.Substring(region.Span.Start, region.Span.Length))
                .ToArray());

        var countPosition = validSource.IndexOf("count", StringComparison.Ordinal) + 2;
        var countRegion = snapshot.FindFragmentRegion(countPosition);
        Assert.NotNull(countRegion);
        Assert.Equal(MacroFragmentKind.Expression, countRegion.Kind);
        Assert.Equal("count", validSource.Substring(countRegion.Span.Start, countRegion.Span.Length));

        const string invalidSource = """
            let view = Html! {
                <h1>Broken</h2>
            }
            """;
        var invalidTree = SyntaxTree.ParseText(invalidSource, path: "invalid-html.rvn");
        var invalidCompilation = CreateConsumerCompilation(invalidTree, macroReference);
        var invalidInvocation = invalidTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var invalidSemanticModel = invalidCompilation.GetSemanticModel(invalidTree);
        var invalidExpansion = invalidSemanticModel.GetMacroExpansion(invalidInvocation);
        var macroDiagnostic = Assert.Single(invalidExpansion!.MacroDiagnostics);

        Assert.Equal("HTML001", macroDiagnostic.Code);
        Assert.Same(invalidTree, macroDiagnostic.Location!.SourceTree);
        Assert.True(invalidInvocation.TokenTree!.Span.Contains(macroDiagnostic.Location.SourceSpan));
        Assert.Contains(
            invalidCompilation.GetDiagnostics(),
            diagnostic =>
                diagnostic.Severity == DiagnosticSeverity.Error &&
                ReferenceEquals(diagnostic.Location.SourceTree, invalidTree) &&
                diagnostic.GetMessage().Contains("HTML001", StringComparison.Ordinal));
    }

    private static Compilation CreateConsumerCompilation(
        SyntaxTree tree,
        MacroReference macroReference)
        => Compilation.Create(
                $"HtmlMacroConsumer_{Guid.NewGuid():N}",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(macroReference);

    private static MacroReference CreateCheckedInHtmlMacroReference()
    {
        var repositoryRoot = Path.GetFullPath(
            Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        var sourcePath = Path.Combine(
            repositoryRoot,
            "samples",
            "projects",
            "macro-html-blazor",
            "macros",
            "HtmlMacro.rvn");
        var source = File.ReadAllText(sourcePath);
        var macroTree = SyntaxTree.ParseText(source, path: sourcePath);
        var codeAnalysisReference = MetadataReference.CreateFromFile(
            typeof(IMacroDefinition).Assembly.Location);
        var macroCompilation = Compilation.Create(
                $"CheckedInHtmlMacro_{Guid.NewGuid():N}",
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
            display: "checked-in HTML macro sample");
    }
}
