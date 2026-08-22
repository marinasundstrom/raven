using System.Collections.Immutable;
using System.Diagnostics;

using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer.Tests;

public sealed class LanguageServerMacroInlayHintTests
{
    [Fact]
    public void AddMacroFragmentTypeHints_ReportsCollectionComprehensionTarget()
    {
        const string code = """
            import Raven.LanguageServer.Tests.*

            class Customer {}

            func Main() {
                let customers = [Customer()]
                let values = fragmentInlay! { [for customer in customers => customer] }
            }
            """;
        var trustedPlatformAssemblies = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!)
            .Split(Path.PathSeparator)
            .Select(MetadataReference.CreateFromFile)
            .Cast<MetadataReference>()
            .ToArray();
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroFragmentInlayHints",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(trustedPlatformAssemblies)
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new FragmentInlayMacro()));
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var sourceText = syntaxTree.GetText();
        var invocation = root.DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        semanticModel.GetMacroFragmentInferredTypeAnnotations(invocation).ShouldHaveSingleItem();
        var hints = new List<InlayHint>();
        var budget = new InlayHintHandler.InlayHintCollectionBudget(
            Stopwatch.StartNew(),
            CancellationToken.None,
            double.PositiveInfinity,
            includeTooltips: false);

        InlayHintHandler.AddMacroFragmentTypeHints(
            hints,
            semanticModel,
            root,
            sourceText,
            root.FullSpan,
            budget,
            CancellationToken.None);

        var hint = Assert.Single(hints);
        hint.Label.String.ShouldBe(": Customer");
        var insertionPosition = code.IndexOf("customer in", StringComparison.Ordinal) + "customer".Length;
        hint.Position.ShouldBe(PositionHelper.ToRange(sourceText, new TextSpan(insertionPosition, 0)).Start);
    }

    [Fact]
    public void AddMacroFragmentTypeHints_ReportsInferredLocalInDeclarationBlock()
    {
        const string code = """
            import Raven.LanguageServer.Tests.*

            component! Greeting(Name: string) {
                let x = 42
            }
            """;
        var trustedPlatformAssemblies = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!)
            .Split(Path.PathSeparator)
            .Select(MetadataReference.CreateFromFile)
            .Cast<MetadataReference>()
            .ToArray();
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroDeclarationFragmentInlayHints",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(trustedPlatformAssemblies)
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new DeclarationFragmentMacro()));
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var sourceText = syntaxTree.GetText();
        var declaration = root.DescendantNodes().OfType<FreestandingMacroDeclarationSyntax>().Single();
        semanticModel.GetMacroFragmentInferredTypeAnnotations(declaration).ShouldHaveSingleItem();
        var hints = new List<InlayHint>();
        var budget = new InlayHintHandler.InlayHintCollectionBudget(
            Stopwatch.StartNew(),
            CancellationToken.None,
            double.PositiveInfinity,
            includeTooltips: false);
        var insertionPosition = code.IndexOf("x = 42", StringComparison.Ordinal) + "x".Length;

        InlayHintHandler.AddMacroFragmentTypeHints(
            hints,
            semanticModel,
            root,
            sourceText,
            new TextSpan(insertionPosition, 0),
            budget,
            CancellationToken.None);

        var hint = Assert.Single(hints);
        hint.Label.String.ShouldBe(": int");
        hint.Position.ShouldBe(PositionHelper.ToRange(sourceText, new TextSpan(insertionPosition, 0)).Start);
    }

    [Fact]
    public void AddMacroFragmentTypeHints_ReportsInferredMatchPatternLocal()
    {
        const string code = """
            import Raven.LanguageServer.Tests.*

            union Command {
                case Add(int)
            }

            func Main() {
                let command: Command = .Add(1)
                let value = fragmentInlay! {
                    match command {
                        .Add(let amount) => amount
                    }
                }
            }
            """;
        var trustedPlatformAssemblies = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!)
            .Split(Path.PathSeparator)
            .Select(MetadataReference.CreateFromFile)
            .Cast<MetadataReference>()
            .ToArray();
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroFragmentMatchPatternInlayHints",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(trustedPlatformAssemblies)
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new FragmentInlayMacro()));
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var sourceText = syntaxTree.GetText();
        var expression = root.DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();
        semanticModel.GetMacroFragmentInferredTypeAnnotations(expression).ShouldHaveSingleItem();
        var hints = new List<InlayHint>();
        var budget = new InlayHintHandler.InlayHintCollectionBudget(
            Stopwatch.StartNew(),
            CancellationToken.None,
            double.PositiveInfinity,
            includeTooltips: false);

        InlayHintHandler.AddMacroFragmentTypeHints(
            hints,
            semanticModel,
            root,
            sourceText,
            root.FullSpan,
            budget,
            CancellationToken.None);

        var hint = Assert.Single(hints);
        hint.Label.String.ShouldBe(": int");
        var insertionPosition = code.IndexOf("amount) =>", StringComparison.Ordinal) + "amount".Length;
        hint.Position.ShouldBe(PositionHelper.ToRange(sourceText, new TextSpan(insertionPosition, 0)).Start);
    }

    private sealed class FragmentInlayMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Name => "fragmentInlay";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            =>
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(0, context.BodySpan.Length))
            ];
    }
}
