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
    public void CheckedInHtmlMacro_RoutesExpressionCompletionThroughReportedFragments()
    {
        var macroReference = CreateCheckedInHtmlMacroReference();
        const string source = """
            class Greeting {
                val message: string = "Hello"

                func Render() => Html! {
                    <h1>{message.}</h1>
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "html-completion.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var position = source.IndexOf("message.", StringComparison.Ordinal) + "message.".Length;
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var items = compilation.GetSemanticModel(syntaxTree)
            .GetCompletions(position)
            .ToArray();
        var messageInfo = compilation.GetMacroFragmentSemanticInfo(
            invocation,
            source.LastIndexOf("message", StringComparison.Ordinal) + 1);

        Assert.Contains(items, static item => item.DisplayText == "Length");
        var message = Assert.IsAssignableFrom<IPropertySymbol>(messageInfo?.SymbolInfo.Symbol);
        Assert.Equal("message", message.Name);
        Assert.Contains(message.Locations, static location => location.IsInSource);
    }

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

    [Fact]
    public void CheckedInHtmlMacro_ProjectsComponentTagSymbol()
    {
        var macroReference = CreateCheckedInHtmlMacroReference();
        const string source = """
            class Greeting { }

            class Gallery {
                func Render() => Html! {
                    <Greeting />
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "html-component-symbol.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var token = Assert.Single(
            compilation.GetMacroTokens(invocation),
            static candidate => candidate.Text == "Greeting");

        var component = Assert.IsAssignableFrom<INamedTypeSymbol>(token.Symbol);
        Assert.Equal("Greeting", component.Name);
        Assert.Contains(component.Locations, static location => location.IsInSource);
    }

    [Fact]
    public void CheckedInHtmlMacro_ResolvesSymbolsInNestedComprehensionTemplate()
    {
        var macroReference = CreateCheckedInHtmlMacroReference();
        const string source = """
            class Todo {
                val Title: string => "Build Raven"
            }

            class TodoList {
                val todos = [Todo()]

                func Render() => Html! {
                    <ul>
                        {[for todo in todos if todo.Title.Length > 0 =>
                            Html! {
                                <li>{todo.Title}</li>
                            }]}
                    </ul>
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "html-comprehension-hover.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .First();
        var todosPosition = source.LastIndexOf("todos if", StringComparison.Ordinal) + 1;
        var todoPosition = source.IndexOf("todo.Title", StringComparison.Ordinal) + 1;
        var titlePosition = source.IndexOf("Title.Length", StringComparison.Ordinal) + 1;
        var nestedTodoPosition = source.LastIndexOf("todo.Title", StringComparison.Ordinal) + 1;
        var nestedTitlePosition = source.LastIndexOf("Title", StringComparison.Ordinal) + 1;

        var todosInfo = compilation.GetMacroFragmentSemanticInfo(invocation, todosPosition);
        var todoInfo = compilation.GetMacroFragmentSemanticInfo(invocation, todoPosition);
        var titleInfo = compilation.GetMacroFragmentSemanticInfo(invocation, titlePosition);
        var nestedTodoInfo = compilation.GetMacroFragmentSemanticInfo(invocation, nestedTodoPosition);
        var nestedTitleInfo = compilation.GetMacroFragmentSemanticInfo(invocation, nestedTitlePosition);

        var todos = Assert.IsAssignableFrom<IPropertySymbol>(todosInfo?.SymbolInfo.Symbol);
        Assert.Equal("todos", todos.Name);

        var todo = Assert.IsAssignableFrom<ILocalSymbol>(todoInfo?.SymbolInfo.Symbol);
        Assert.Equal("todo", todo.Name);
        Assert.Equal("Todo", todo.Type.Name);

        var title = Assert.IsAssignableFrom<IPropertySymbol>(titleInfo?.SymbolInfo.Symbol);
        Assert.Equal("Title", title.Name);
        Assert.Equal(SpecialType.System_String, title.Type.SpecialType);

        var nestedTodo = Assert.IsAssignableFrom<ILocalSymbol>(nestedTodoInfo?.SymbolInfo.Symbol);
        Assert.Equal("todo", nestedTodo.Name);
        Assert.Equal("Todo", nestedTodo.Type.Name);

        var nestedTitle = Assert.IsAssignableFrom<IPropertySymbol>(nestedTitleInfo?.SymbolInfo.Symbol);
        Assert.Equal("Title", nestedTitle.Name);
        Assert.Equal(SpecialType.System_String, nestedTitle.Type.SpecialType);
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
