using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MinimalDslMacroToolingAcceptanceTests
{
    [Fact]
    public void CheckedInGuardMacro_ReportsAndBindsItsEmbeddedRavenExpression()
    {
        var macroReference = CreateCheckedInGuardMacroReference();
        const string source = """
            class GuardHost {
                func ShouldRetry(answer: int) -> bool => guard! {
                    unless answer == 42
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "guard-fragment.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocableMacroExpressionSyntax>()
            .Single();
        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        var region = Assert.Single(semanticModel
            .GetMacroInputSnapshot(invocation)
            .FragmentRegions);
        Assert.Equal(MacroFragmentKind.Expression, region.Kind);
        Assert.Equal(
            "answer == 42",
            source.Substring(region.Span.Start, region.Span.Length).Trim());

        var answerPosition = source.IndexOf("answer ==", StringComparison.Ordinal) + 1;
        var answer = Assert.IsAssignableFrom<IParameterSymbol>(
            compilation.GetMacroFragmentSemanticInfo(invocation, answerPosition)?.SymbolInfo.Symbol);
        Assert.Equal("answer", answer.Name);
        Assert.Equal(SpecialType.System_Int32, answer.Type.SpecialType);
    }

    [Fact]
    public void CheckedInGuardMacro_ReportsMissingFragmentAtTheAuthoredBoundary()
    {
        var macroReference = CreateCheckedInGuardMacroReference();
        const string source = """
            class GuardHost {
                func ShouldRetry() -> bool => guard! {
                    unless
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "guard-diagnostic.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocableMacroExpressionSyntax>()
            .Single();
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        Assert.Single(semanticModel
            .GetMacroInputSnapshot(invocation)
            .FragmentRegions);

        var expansion = semanticModel.GetMacroExpansion(invocation);
        Assert.NotNull(expansion);
        var diagnostics = expansion.MacroDiagnostics;
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("GUARD001", diagnostic.Code);
        Assert.Contains("Raven expression", diagnostic.Message, StringComparison.Ordinal);
        Assert.Equal(syntaxTree, diagnostic.Location?.SourceTree);
        Assert.Equal(
            source.IndexOf("unless", StringComparison.Ordinal) + "unless".Length,
            diagnostic.Location?.SourceSpan.Start);
    }

    private static Compilation CreateConsumerCompilation(
        SyntaxTree tree,
        MacroReference macroReference)
        => Compilation.Create(
                $"MinimalDslConsumer_{Guid.NewGuid():N}",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(macroReference);

    private static MacroReference CreateCheckedInGuardMacroReference()
    {
        var repositoryRoot = Path.GetFullPath(
            Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        var sourcePath = Path.Combine(
            repositoryRoot,
            "samples",
            "projects",
            "macro-dsl",
            "macros",
            "GuardMacro.rvn");
        var macroTree = SyntaxTree.ParseText(File.ReadAllText(sourcePath), path: sourcePath);
        var codeAnalysisReference = MetadataReference.CreateFromFile(
            typeof(IMacroDefinition).Assembly.Location);
        var macroCompilation = Compilation.Create(
                $"CheckedInGuardMacro_{Guid.NewGuid():N}",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(macroTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddReferences(codeAnalysisReference);

        using var image = new MemoryStream();
        var emitResult = macroCompilation.Emit(image);
        Assert.True(
            emitResult.Success,
            string.Join(Environment.NewLine, emitResult.Diagnostics));

        return MacroReference.CreateFromImage(
            image.ToArray(),
            display: "checked-in minimal DSL macro sample");
    }
}
