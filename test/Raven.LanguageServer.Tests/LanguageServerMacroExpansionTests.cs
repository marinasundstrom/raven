using System.Collections.Immutable;

using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

public sealed class LanguageServerMacroExpansionTests
{
    [Fact]
    public void MacroExpansionDisplayService_BuildsPreviewAtAttributePosition()
    {
        const string code = """
class MyViewModel {
    #[Observable]
    var Title: string
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rvn");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(typeof(ObservableMacroPlugin)));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var sourceText = SourceText.From(code);
        var root = syntaxTree.GetRoot();
        var attribute = root.DescendantNodes().OfType<AttributeSyntax>().Single();

        var success = MacroExpansionDisplayService.TryCreateForOffset(
            sourceText,
            semanticModel,
            root,
            attribute.Span.Start + 2,
            out var display);

        success.ShouldBeTrue();
        display.MacroName.ShouldBe("Observable");
        display.FullText.ShouldContain("private field _Title: string");
        display.FullText.ShouldContain("var Title: string");
        display.PreviewText.ShouldContain("_Title");
    }

    [Fact]
    public void MacroExpansionDisplayService_BuildsPreviewForRequestedRange()
    {
        const string code = """
class MyViewModel {
    #[Observable]
    var Title: string
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rvn");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(typeof(ObservableMacroPlugin)));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var sourceText = SourceText.From(code);
        var root = syntaxTree.GetRoot();
        var attribute = root.DescendantNodes().OfType<AttributeSyntax>().Single();
        var selectionStart = attribute.Span.Start + 2;
        var selectionEnd = attribute.Span.End - 1;
        var (startLine, startColumn) = sourceText.GetLineAndColumn(selectionStart);
        var (endLine, endColumn) = sourceText.GetLineAndColumn(selectionEnd);

        var success = MacroExpansionDisplayService.TryCreateForRange(
            sourceText,
            semanticModel,
            root,
            new LspRange(
                new Position(startLine - 1, startColumn - 1),
                new Position(endLine - 1, endColumn - 1)),
            out var display);

        success.ShouldBeTrue();
        display.Span.ShouldBe(attribute.Span);
        display.FullText.ShouldContain("set {");
    }

    public sealed class ObservableMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ObservableMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ObservableMacro()];
    }

    public sealed class ObservableMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var tree = SyntaxFactory.ParseSyntaxTree("""
                class __GeneratedContainer {
                    private field _Title: string

                    var Title: string {
                        get => _Title
                        set {
                            _Title = value
                        }
                    }
                }
                """);

            var container = (ClassDeclarationSyntax)tree.GetRoot().Members.Single();
            return new MacroExpansionResult
            {
                IntroducedMembers = [(FieldDeclarationSyntax)container.Members[0]],
                ReplacementDeclaration = (PropertyDeclarationSyntax)container.Members[1]
            };
        }
    }
}
