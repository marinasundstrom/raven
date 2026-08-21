using System;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroEmbeddedLanguageProjectionTests
{
    [Fact]
    public void GetProjection_ContainsOptionalProviderFailure()
    {
        var (compilation, invocation) = CreateCompilation(new FailingProjectionMacro());

        var projection = compilation.GetMacroEmbeddedLanguageProjection(invocation);

        Assert.Null(projection);
    }

    [Fact]
    public void GetProjection_RejectsChangedLineBreakPositions()
    {
        var (compilation, invocation) = CreateCompilation(new ChangedLineBreakProjectionMacro());

        var projection = compilation.GetMacroEmbeddedLanguageProjection(invocation);

        Assert.Null(projection);
    }

    [Fact]
    public void GetProjection_PropagatesRequestCancellation()
    {
        var (compilation, invocation) = CreateCompilation(new CancellableProjectionMacro());
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();

        Assert.Throws<OperationCanceledException>(
            () => compilation.GetMacroEmbeddedLanguageProjection(invocation, cancellation.Token));
    }

    private static (Compilation Compilation, FreestandingMacroExpressionSyntax Invocation) CreateCompilation(
        IMacroDefinition macro)
    {
        const string source = """
            let value = projection! {
                <p>{message}</p>
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "projection.rvn");
        var compilation = Compilation.Create(
                "projection-tests",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(macro));
        var invocation = Assert.Single(
            syntaxTree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>());
        return (compilation, invocation);
    }

    private abstract class ProjectionMacro : IMacroDefinition, IMacroEmbeddedLanguageProvider
    {
        public string Namespace => string.Empty;

        public string Name => "projection";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public abstract MacroEmbeddedLanguageProjection? GetEmbeddedLanguageProjection(
            TokenTreeMacroContext context);
    }

    private sealed class FailingProjectionMacro : ProjectionMacro
    {
        public override MacroEmbeddedLanguageProjection? GetEmbeddedLanguageProjection(
            TokenTreeMacroContext context)
            => throw new InvalidOperationException("Optional tooling failure");
    }

    private sealed class ChangedLineBreakProjectionMacro : ProjectionMacro
    {
        public override MacroEmbeddedLanguageProjection? GetEmbeddedLanguageProjection(
            TokenTreeMacroContext context)
            => context.CreateEmbeddedLanguageProjection(
                "html",
                context.GetBodyText().Replace('\n', ' '));
    }

    private sealed class CancellableProjectionMacro : ProjectionMacro
    {
        public override MacroEmbeddedLanguageProjection? GetEmbeddedLanguageProjection(
            TokenTreeMacroContext context)
        {
            context.CancellationToken.ThrowIfCancellationRequested();
            return context.CreateEmbeddedLanguageProjection("html", context.GetBodyText());
        }
    }
}
