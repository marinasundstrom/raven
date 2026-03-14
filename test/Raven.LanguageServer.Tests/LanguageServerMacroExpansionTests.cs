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

    [Fact]
    public void MacroExpansionDisplayService_FormatsDetachedSyntaxFactoryExpansion()
    {
        const string code = """
class MyViewModel {
    #[Observable]
    var Title: string = ""
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rvn");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(typeof(DetachedObservableMacroPlugin)));

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
        display.FullText.ShouldContain("private var _Title: string = \"\"");
        display.FullText.ShouldContain("var Title: string");
        display.FullText.ShouldContain("get => _Title");
        display.FullText.ShouldContain("set {");
        display.FullText.ShouldNotContain("privatevar_Title");
        display.FullText.ShouldNotContain("valoldValue");
    }

    [Fact]
    public void MacroExpansionDisplayService_BuildsPreviewForFreestandingMacroExpression()
    {
        const string code = """
class Harness {
    func Run() -> int => #answer()
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rvn");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(typeof(FreestandingMacroPlugin)));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var sourceText = SourceText.From(code);
        var root = syntaxTree.GetRoot();
        var expression = root.DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();

        var success = MacroExpansionDisplayService.TryCreateForOffset(
            sourceText,
            semanticModel,
            root,
            expression.Span.Start + 1,
            out var display);

        success.ShouldBeTrue();
        display.MacroName.ShouldBe("answer");
        display.InvocationDisplay.ShouldBe("#answer(...)");
        display.FullText.ShouldBe("42");
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

    public sealed class DetachedObservableMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "DetachedObservableMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new DetachedObservableMacro()];
    }

    public sealed class DetachedObservableMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public MacroKind Kind => MacroKind.AttachedDeclaration;

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var property = (PropertyDeclarationSyntax)context.TargetDeclaration;
            var propertyName = property.Identifier.ValueText;
            var backingFieldName = "_" + propertyName;
            var propertyType = property.Type;

            var backingField = SyntaxFactory.PropertyDeclaration(
                SyntaxFactory.List<AttributeListSyntax>(),
                SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PrivateKeyword)),
                SyntaxFactory.Token(SyntaxKind.VarKeyword),
                explicitInterfaceSpecifier: null,
                SyntaxFactory.Identifier(backingFieldName),
                propertyType,
                accessorList: null,
                expressionBody: null,
                initializer: property.Initializer,
                terminatorToken: SyntaxFactory.Token(SyntaxKind.None));

            var replacement = property
                .WithAttributeLists(SyntaxFactory.List<AttributeListSyntax>())
                .WithAccessorList(SyntaxFactory.AccessorList(
                    SyntaxFactory.List<AccessorDeclarationSyntax>(
                    [
                        SyntaxFactory.AccessorDeclaration(
                            SyntaxKind.GetAccessorDeclaration,
                            SyntaxFactory.List<AttributeListSyntax>(),
                            SyntaxFactory.TokenList(),
                            SyntaxFactory.Token(SyntaxKind.GetKeyword),
                            body: null,
                            expressionBody: SyntaxFactory.ArrowExpressionClause(SyntaxFactory.IdentifierName(backingFieldName)),
                            terminatorToken: SyntaxFactory.Token(SyntaxKind.None)),
                        SyntaxFactory.AccessorDeclaration(
                            SyntaxKind.SetAccessorDeclaration,
                            SyntaxFactory.List<AttributeListSyntax>(),
                            SyntaxFactory.TokenList(),
                            SyntaxFactory.Token(SyntaxKind.SetKeyword),
                            body: SyntaxFactory.BlockStatement(
                                SyntaxFactory.List<StatementSyntax>(
                                [
                                    SyntaxFactory.LocalDeclarationStatement(
                                        SyntaxFactory.VariableDeclaration(
                                            SyntaxFactory.Token(SyntaxKind.ValKeyword),
                                            SyntaxFactory.SeparatedList<VariableDeclaratorSyntax>(
                                            [
                                                new SyntaxNodeOrToken(SyntaxFactory.VariableDeclarator(
                                                    SyntaxFactory.Identifier("oldValue"),
                                                    typeAnnotation: null,
                                                    initializer: SyntaxFactory.EqualsValueClause(SyntaxFactory.IdentifierName(backingFieldName))))
                                            ]))),
                                    SyntaxFactory.AssignmentStatement(
                                        SyntaxKind.SimpleAssignmentStatement,
                                        SyntaxFactory.IdentifierName(backingFieldName),
                                        SyntaxFactory.Token(SyntaxKind.EqualsToken),
                                        SyntaxFactory.IdentifierName("value")),
                                    SyntaxFactory.ExpressionStatement(
                                        SyntaxFactory.InvocationExpression(
                                            SyntaxFactory.IdentifierName("RaisePropertyChanged"),
                                            SyntaxFactory.ArgumentList(
                                                SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                                [
                                                    new SyntaxNodeOrToken(SyntaxFactory.Argument(SyntaxFactory.NameOfExpression(SyntaxFactory.IdentifierName(propertyName)))),
                                                    new SyntaxNodeOrToken(SyntaxFactory.Token(SyntaxKind.CommaToken)),
                                                    new SyntaxNodeOrToken(SyntaxFactory.Argument(SyntaxFactory.IdentifierName("oldValue"))),
                                                    new SyntaxNodeOrToken(SyntaxFactory.Token(SyntaxKind.CommaToken)),
                                                    new SyntaxNodeOrToken(SyntaxFactory.Argument(SyntaxFactory.IdentifierName("value")))
                                                ]))))
                                ])),
                            expressionBody: null,
                            terminatorToken: SyntaxFactory.Token(SyntaxKind.None))
                    ])))
                .WithExpressionBody(null)
                .WithInitializer(null)
                .WithTerminatorToken(SyntaxFactory.Token(SyntaxKind.None));

            return new MacroExpansionResult
            {
                IntroducedMembers = [backingField],
                ReplacementDeclaration = replacement
            };
        }
    }

    public sealed class FreestandingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "FreestandingMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new AnswerMacro()];
    }

    public sealed class AnswerMacro : IFreestandingExpressionMacro
    {
        public string Name => "answer";
        public MacroKind Kind => MacroKind.FreestandingExpression;
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
            => new()
            {
                Expression = SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(42))
            };
    }
}
