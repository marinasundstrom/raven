using System.Collections.Immutable;
using System.Reflection;

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
        display.FullText.ShouldContain("private field");
        display.FullText.ShouldContain("_Title: string");
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
    public void MacroExpansionDisplayService_ShowsComposedExpansionForStackedMacros()
    {
        const string code = """
class MyViewModel {
    #[First]
    #[Second]
    var Title: string
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rvn");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(typeof(StackingMacroPlugin)));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var sourceText = SourceText.From(code);
        var root = syntaxTree.GetRoot();
        var attribute = root.DescendantNodes().OfType<AttributeSyntax>().First();

        var success = MacroExpansionDisplayService.TryCreateForOffset(
            sourceText,
            semanticModel,
            root,
            attribute.Span.Start + 2,
            out var display);

        success.ShouldBeTrue();
        display.FullText.ShouldContain("func Before_Title() -> int");
        display.FullText.ShouldContain("var Second_First_Title: string");
        display.FullText.ShouldContain("func AfterAgain_First_Title() -> int");
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

    [Fact]
    public void MacroExpansionDisplayService_DoesNotBuildPreviewInsideFreestandingMacroArguments()
    {
        const string code = """
class Harness {
    func Run(viewModel: CounterViewModel) {
        val subscription = #subscribe(viewModel.Count, (value) => {
            WriteLine(value)
        })
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rvn");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(typeof(ReactiveSubscribeMacroPlugin)));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var sourceText = SourceText.From(code);
        var root = syntaxTree.GetRoot();
        var argumentOffset = code.IndexOf("Count", StringComparison.Ordinal) + 1;

        var success = MacroExpansionDisplayService.TryCreateForOffset(
            sourceText,
            semanticModel,
            root,
            argumentOffset,
            out _);

        success.ShouldBeFalse();
    }

    [Fact]
    public void HoverHandler_MacroHint_IncludesKindTargetsAndArguments()
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

        var tryGetMacroHint = typeof(HoverHandler)
            .GetMethod("TryGetMacroHint", BindingFlags.NonPublic | BindingFlags.Static)!;
        var arguments = new object?[] { compilation, "Observable", null };

        var success = (bool)tryGetMacroHint.Invoke(null, arguments)!;
        var hint = (string)arguments[2]!;

        success.ShouldBeTrue();
        hint.ShouldContain("Attached declaration macro.");
        hint.ShouldContain("Targets `Property`.");
        hint.ShouldContain("No arguments.");
    }

    [Fact]
    public void SymbolResolver_FreestandingMacroLambdaArgument_ResolvesLambdaParameter()
    {
        const string code = """
import System.*

class ObservableInt {
    func Subscribe(handler: (int) -> unit) -> unit { }
}

class CounterViewModel {
    var Count: int = 0
    val CountChanged: ObservableInt = ObservableInt()
}

class Harness {
    func WriteLine(value: int) -> unit { }

    func Run(viewModel: CounterViewModel) -> unit {
        val subscription = #subscribe(viewModel.Count, (value) => {
            WriteLine(value)
        })
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rvn");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(typeof(ArgumentAwareFreestandingMacroPlugin)))
            .AddReferences(LanguageServerTestReferences.Default);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        compilation.GetDiagnostics().Where(d => d.Severity == Raven.CodeAnalysis.DiagnosticSeverity.Error).ShouldBeEmpty();
        var root = syntaxTree.GetRoot();
        var valueReference = root.DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(static identifier => identifier.Identifier.ValueText == "value")
            .Last();

        var resolution = SymbolResolver.ResolveSymbolAtPosition(semanticModel, root, valueReference.Identifier.SpanStart + 1);

        resolution.ShouldNotBeNull();
        var parameterSymbol = resolution!.Value.Symbol.ShouldBeAssignableTo<IParameterSymbol>();
        parameterSymbol.Name.ShouldBe("value");
        parameterSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("int");
    }

    [Fact]
    public void SymbolResolver_AttachedMacroPropertyReference_PrefersReceiverBoundProperty()
    {
        const string code = """
import System.*
import System.Console.*

class MyViewModel {
    #[Observable]
    var Title: string
}

class Program {
    static func Main() -> unit {
        val viewModel = MyViewModel()
        viewModel.Title = "Hello from Raven"
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rvn");
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(typeof(ObservableMacroPlugin)))
            .AddReferences(LanguageServerTestReferences.Default);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var titleReference = root.DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(static access => access.ToString() == "viewModel.Title");

        var resolution = SymbolResolver.ResolveSymbolAtPosition(
            semanticModel,
            root,
            titleReference.Name.Span.Start + 1);

        resolution.ShouldNotBeNull();
        var propertySymbol = resolution!.Value.Symbol.ShouldBeAssignableTo<IPropertySymbol>();
        propertySymbol.Name.ShouldBe("Title");
        propertySymbol.ContainingType?.Name.ShouldBe("MyViewModel");
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

            var backingField = SyntaxFactory.StoredPropertyDeclaration(
                SyntaxFactory.List<AttributeListSyntax>(),
                SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PrivateKeyword)),
                SyntaxFactory.Token(SyntaxKind.VarKeyword),
                SyntaxFactory.Identifier(backingFieldName),
                propertyType,
                property.Initializer);

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
                            SyntaxFactory.ArrowExpressionClause(SyntaxFactory.IdentifierName(backingFieldName))),
                        SyntaxFactory.AccessorDeclaration(
                            SyntaxKind.SetAccessorDeclaration,
                            SyntaxFactory.List<AttributeListSyntax>(),
                            SyntaxFactory.TokenList(),
                            SyntaxFactory.Token(SyntaxKind.SetKeyword),
                            SyntaxFactory.BlockStatement(
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
                                ])))
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

    public sealed class ArgumentAwareFreestandingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ArgumentAwareFreestandingMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new SubscribeMacro()];
    }

    public sealed class SubscribeMacro : IFreestandingExpressionMacro
    {
        public string Name => "subscribe";
        public MacroKind Kind => MacroKind.FreestandingExpression;
        public MacroTarget Targets => MacroTarget.None;
        public bool AcceptsArguments => true;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
        {
            var propertyAccess = (MemberAccessExpressionSyntax)context.Arguments[0].Expression;
            var callback = context.Arguments[1].Expression;
            var propertyIdentifier = (IdentifierNameSyntax)propertyAccess.Name;
            var signalName = propertyIdentifier.Identifier.ValueText + "Changed";

            return new FreestandingMacroExpansionResult
            {
                Expression = SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            propertyAccess.Expression,
                            SyntaxFactory.Token(SyntaxKind.DotToken),
                            SyntaxFactory.IdentifierName(signalName)),
                        SyntaxFactory.Token(SyntaxKind.DotToken),
                        SyntaxFactory.IdentifierName("Subscribe")),
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        [
                            new SyntaxNodeOrToken(SyntaxFactory.Argument(callback))
                        ])))
            };
        }
    }

    public sealed class ReactiveSubscribeMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "ReactiveSubscribeMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ReactiveSubscribeMacro()];
    }

    public sealed class ReactiveSubscribeMacro : IFreestandingExpressionMacro
    {
        public string Name => "subscribe";
        public MacroKind Kind => MacroKind.FreestandingExpression;
        public MacroTarget Targets => MacroTarget.None;
        public bool AcceptsArguments => true;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
        {
            var propertyAccess = (MemberAccessExpressionSyntax)context.Arguments[0].Expression;
            var callback = context.Arguments[1].Expression;
            var propertyIdentifier = (IdentifierNameSyntax)propertyAccess.Name;
            var signalName = propertyIdentifier.Identifier.ValueText + "Changed";

            return new FreestandingMacroExpansionResult
            {
                Expression = SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            propertyAccess.Expression,
                            SyntaxFactory.Token(SyntaxKind.DotToken),
                            SyntaxFactory.IdentifierName(signalName)),
                        SyntaxFactory.Token(SyntaxKind.DotToken),
                        SyntaxFactory.IdentifierName("Subscribe")),
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        [
                            new SyntaxNodeOrToken(SyntaxFactory.Argument(callback))
                        ])))
            };
        }
    }

    public sealed class StackingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => "StackingMacroPlugin";

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new FirstStackingMacro(), new SecondStackingMacro()];
    }

    public sealed class FirstStackingMacro : IAttachedDeclarationMacro
    {
        public string Name => "First";
        public MacroKind Kind => MacroKind.AttachedDeclaration;
        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var property = (PropertyDeclarationSyntax)context.TargetDeclaration;
            var members = ParseMembers($$"""
                class __GeneratedContainer {
                    func Before_{{property.Identifier.ValueText}}() -> int { return 1 }
                    var First_{{property.Identifier.ValueText}}: string { get => "" }
                }
                """);

            return new MacroExpansionResult
            {
                IntroducedMembers = [members[0]],
                ReplacementDeclaration = members[1]
            };
        }
    }

    public sealed class SecondStackingMacro : IAttachedDeclarationMacro
    {
        public string Name => "Second";
        public MacroKind Kind => MacroKind.AttachedDeclaration;
        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var property = (PropertyDeclarationSyntax)context.CurrentDeclaration;
            var members = ParseMembers($$"""
                class __GeneratedContainer {
                    var Second_{{property.Identifier.ValueText}}: string { get => "" }
                    func AfterAgain_{{property.Identifier.ValueText}}() -> int { return 2 }
                }
                """);

            return new MacroExpansionResult
            {
                ReplacementDeclaration = members[0],
                PeerDeclarations = [members[1]]
            };
        }
    }

    private static ImmutableArray<MemberDeclarationSyntax> ParseMembers(string source)
    {
        var tree = SyntaxFactory.ParseSyntaxTree(source);
        var container = (ClassDeclarationSyntax)tree.GetRoot().Members.Single();
        return [.. container.Members];
    }
}
