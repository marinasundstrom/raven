using System.Collections.Immutable;
using System.Diagnostics;
using System.Reflection;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.LanguageServer.Tests;

public sealed class LanguageServerMemberMacroTests
{
    [Theory]
    [InlineData("signature")]
    [InlineData("hover-name")]
    [InlineData("hover-local")]
    [InlineData("expansion")]
    [InlineData("inlay")]
    [InlineData("definition")]
    [InlineData("tokens")]
    public async Task TypeMemberMacro_ProvidesEditorFeatures(string feature)
    {
        const string code = """
            class Host {
                members!(count: 1) {
                    let value = 42
                    System.Console.WriteLine(value)
                }
            }
            """;
        var path = Path.Combine(Path.GetTempPath(), $"raven-member-{Guid.NewGuid():N}.rvn");
        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams());
        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        var uri = DocumentUri.FromFileSystemPath(path);
        var document = await store.UpsertDocumentAsync(uri, code);
        workspace.TryApplyChanges(workspace.CurrentSolution.AddMacroReference(
            document.Project.Id, new MacroReference(new MemberMacro()))).ShouldBeTrue();
        var model = await store.GetSemanticModelAsync(uri, CancellationToken.None);
        model.ShouldNotBeNull();
        var root = model.SyntaxTree.GetRoot();
        var text = SourceText.From(code);
        var nameOffset = code.IndexOf("members", StringComparison.Ordinal) + 1;
        var localOffset = code.LastIndexOf("value", StringComparison.Ordinal) + 1;
        Position PositionAt(int offset) => PositionHelper.ToRange(text, new TextSpan(offset, 0)).Start;

        switch (feature)
        {
            case "signature":
                var signature = await new SignatureHelpHandler(store, NullLogger<SignatureHelpHandler>.Instance)
                    .Handle(new SignatureHelpParams
                    {
                        TextDocument = new TextDocumentIdentifier(uri),
                        Position = PositionAt(code.IndexOf("1)", StringComparison.Ordinal))
                    }, CancellationToken.None);
                signature.ShouldNotBeNull();
                signature.Signatures.Single().Label.ShouldBe("members!(count: int) { ... }");
                break;
            case "hover-name":
            case "hover-local":
                var hover = await new HoverHandler(store, NullLogger<HoverHandler>.Instance)
                    .Handle(new HoverParams
                    {
                        TextDocument = new TextDocumentIdentifier(uri),
                        Position = PositionAt(feature == "hover-name" ? nameOffset : localOffset)
                    }, CancellationToken.None);
                hover.ShouldNotBeNull();
                hover.Contents.MarkupContent!.Value.ShouldContain(feature == "hover-name" ? "Macro `members!" : "value: int");
                break;
            case "expansion":
                var member = root.DescendantNodes().OfType<FreestandingMacroMemberDeclarationSyntax>().Single();
                var expansion = model.GetMacroExpansion(member);
                expansion.ShouldNotBeNull(string.Join("\n", model.Compilation.GetDiagnostics()));
                (expansion.Node?.ToFullString() ?? string.Join("\n", expansion.Members)).ShouldContain("Generated");
                model.GetDiagnostics().Where(d => d.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error).ShouldBeEmpty();
                MacroExpansionDisplayService.TryCreateForOffset(text, model, root, nameOffset, out var display).ShouldBeTrue();
                display.FullText.ShouldContain("Generated");
                MacroExpansionDisplayService.TryCreateForRange(text, model, root,
                    PositionHelper.ToRange(text, new TextSpan(nameOffset, 0)), out _).ShouldBeTrue();
                MacroExpansionDisplayService.TryCreateForOffset(text, model, root, localOffset, out _).ShouldBeFalse();
                break;
            case "inlay":
                var hints = new List<InlayHint>();
                var budget = new InlayHintHandler.InlayHintCollectionBudget(
                    Stopwatch.StartNew(), CancellationToken.None, double.PositiveInfinity, includeTooltips: false);
                InlayHintHandler.AddMacroFragmentTypeHints(hints, model, root, text, root.FullSpan, budget, CancellationToken.None);
                hints.ShouldNotBeEmpty();
                break;
            case "definition":
                var resolve = typeof(DefinitionHandler).GetMethod("TryResolveMacroFragmentDefinition", BindingFlags.NonPublic | BindingFlags.Static)!;
                var info = (MacroFragmentSemanticInfo?)resolve.Invoke(null, [model, root, localOffset, CancellationToken.None]);
                info.ShouldNotBeNull();
                info.SymbolInfo.Symbol.ShouldBeAssignableTo<ILocalSymbol>().Name.ShouldBe("value");
                break;
            case "tokens":
                var classify = typeof(SemanticTokensHandler).GetMethod("CreateMacroTokenEntries", BindingFlags.NonPublic | BindingFlags.Static)!;
                var entries = (Array)classify.Invoke(null, [root, model, CancellationToken.None])!;
                entries.Length.ShouldBeGreaterThan(0);
                break;
        }
    }

    private sealed class MemberMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Name => "members";
        public string Namespace => string.Empty;
        public MacroInvocationTargets InvocationTargets => MacroInvocationTargets.TypeMember;

        public MemberDeclarationSyntax Expand(int count, TokenTreeMacroContext context)
            => SyntaxFactory.ParseMemberDeclaration("class Generated {}")!;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            => [context.CreateFragmentRegion(MacroFragmentKind.Block, new TextSpan(0, context.BodySpan.Length))];
    }
}
