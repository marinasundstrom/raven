using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MarkupMacroToolingAcceptanceTests
{
    [Fact]
    public void CheckedInFunctionComponent_ComposesOrdinaryRavenWithMarkupMacro()
    {
        var macroReference = CreateCheckedInBlazorMacroReference();
        const string source = """
            public component! Greeting(Name: string = "") {
                let x = 42

                markup! {
                    <section class="greeting">
                        <h1>Hello {Name}</h1>
                    </section>
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "function-component.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference)
            .AddReferences(CreateAspNetCoreComponentsReference());
        var expanded = compilation.GetSemanticModel(syntaxTree).GetExpandedRoot().ToFullString();

        Assert.Contains("class Greeting", expanded, StringComparison.Ordinal);
        Assert.Contains("let x = 42", expanded, StringComparison.Ordinal);
        Assert.Contains("BuildRenderTree", expanded, StringComparison.Ordinal);
        Assert.DoesNotContain("markup!", expanded, StringComparison.Ordinal);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void CheckedInFunctionComponent_RemainsValidWhenBlankLinesAreAppended()
    {
        var macroReference = CreateCheckedInBlazorMacroReference();
        var source = """
            import System.Console.*

            component! Greeting(Name: string = "") {
                WriteLine("Rendering Greeting for ${Name}")

                markup! {
                    <section class="greeting">
                        <h1>Hello {Name}</h1>
                    </section>
                }
            }
            """ + "\n";
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "function-component-edit",
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            targetFramework: TestMetadataReferences.TargetFramework);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        foreach (var reference in TestMetadataReferences.Default)
            project = project.AddMetadataReference(reference);
        project = project
            .AddMetadataReference(CreateAspNetCoreComponentsReference())
            .AddMacroReference(macroReference);
        var document = project.AddDocument(
            "Greeting.rvn",
            SourceText.From(source),
            "/tmp/Greeting.rvn");
        workspace.TryApplyChanges(document.Project.Solution);

        var compilation = workspace.GetCompilation(projectId);
        var syntaxTree = Assert.Single(compilation.SyntaxTrees);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        for (var index = 0; index < 3; index++)
        {
            var currentText = syntaxTree.GetText();
            var updatedText = currentText.Replace(currentText.Length, 0, "\n");
            workspace.TryApplyChanges(
                workspace.CurrentSolution.WithDocumentText(document.Id, updatedText));
            compilation = workspace.GetCompilation(projectId);
            syntaxTree = Assert.Single(compilation.SyntaxTrees);

            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            var declaration = Assert.Single(
                syntaxTree.GetRoot().Members.OfType<FreestandingMacroDeclarationSyntax>());
            Assert.IsType<CompilationUnitSyntax>(declaration.Parent);
            Assert.NotNull(semanticModel.GetMacroInputSnapshot(declaration));
            _ = semanticModel.GetMacroFragmentInferredTypeAnnotations(declaration);
            Assert.DoesNotContain(
                compilation.GetDiagnostics(),
                static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        }
    }

    [Fact]
    public void CheckedInFunctionComponent_InFileScopedNamespaceRemainsADeclarationMember()
    {
        var macroReference = CreateCheckedInBlazorMacroReference();
        const string source = """
            import System.Console.*

            namespace Components;

            component! Greeting(Name: string = "") {
                WriteLine("Rendering Greeting for ${Name}")

                markup! {
                    <section class="greeting">
                        <h1>Hello {Name}</h1>
                    </section>
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "namespaced-function-component.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference)
            .AddReferences(CreateAspNetCoreComponentsReference());
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var declaration = Assert.Single(
            syntaxTree.GetRoot().DescendantNodes().OfType<FreestandingMacroDeclarationSyntax>());

        Assert.IsType<FileScopedNamespaceDeclarationSyntax>(declaration.Parent);
        Assert.Empty(declaration.Ancestors().OfType<GlobalStatementSyntax>());

        _ = semanticModel.GetMacroFragmentInferredTypeAnnotations(declaration);
        var expanded = semanticModel.GetExpandedRoot().ToFullString();

        Assert.Contains("class Greeting", expanded, StringComparison.Ordinal);
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void CheckedInFunctionComponent_ExposesBlockAndNestedMarkupTooling()
    {
        var macroReference = CreateCheckedInBlazorMacroReference();
        const string source = """
            component! Greeting(Name: string = "") {
                let message = Name

                markup! {
                    <h1>Hello {Name.}</h1>
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "function-component-tooling.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference)
            .AddReferences(CreateAspNetCoreComponentsReference());
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var declaration = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroDeclarationSyntax>()
            .Single();

        var snapshot = semanticModel.GetMacroInputSnapshot(declaration);
        var block = Assert.Single(snapshot.FragmentRegions);
        Assert.Equal(MacroFragmentKind.Block, block.Kind);
        var rootClassifications = SemanticClassifier.Classify(
            syntaxTree.GetRoot(),
            semanticModel,
            allowBinding: false);
        Assert.Contains(
            rootClassifications.Tokens,
            static pair => pair.Key.ValueText == "component" && pair.Value == SemanticClassification.Keyword);
        var fragmentClassifications = semanticModel.GetMacroFragmentClassifications(declaration);
        Assert.Contains(
            fragmentClassifications.Tokens,
            static pair => pair.Key.ValueText == "markup" && pair.Value == SemanticClassification.Keyword);
        var reportedParameter = Assert.Single(block.Locals);
        Assert.True(reportedParameter.IsParameter);
        Assert.Equal("Name", reportedParameter.Name);
        var directNamePosition = source.IndexOf("= Name", StringComparison.Ordinal) + "= ".Length + 1;
        var directInfo = semanticModel.GetMacroFragmentSemanticInfo(declaration, directNamePosition);
        Assert.IsAssignableFrom<IParameterSymbol>(directInfo?.SymbolInfo.Symbol);
        var namePosition = source.LastIndexOf("Name.", StringComparison.Ordinal) + 1;
        var info = semanticModel.GetMacroFragmentSemanticInfo(declaration, namePosition);
        var parameter = Assert.IsAssignableFrom<IParameterSymbol>(info?.SymbolInfo.Symbol);
        Assert.Equal("Name", parameter.Name);
        Assert.Equal(SpecialType.System_String, parameter.Type.SpecialType);
        Assert.Contains(parameter.Locations, static location => location.IsInSource);

        var completionPosition = source.LastIndexOf("Name.", StringComparison.Ordinal) + "Name.".Length;
        var completions = semanticModel.GetCompletions(completionPosition);
        Assert.Contains(completions, static item => item.DisplayText == "Length");

    }

    [Fact]
    public void CheckedInMarkupMacro_RoutesExpressionCompletionThroughReportedFragments()
    {
        var macroReference = CreateCheckedInMarkupMacroReference();
        const string source = """
            class Greeting {
                val message: string = "Hello"

                func Render() => Markup! {
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
    public void CheckedInMarkupMacro_ProvidesCompleteToolingSnapshotAndAuthoredDiagnostics()
    {
        var macroReference = CreateCheckedInMarkupMacroReference();
        const string validSource = """
            let view = Markup! {
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
            let view = Markup! {
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
    public void CheckedInMarkupMacro_ProjectsComponentTagSymbol()
    {
        var macroReference = CreateCheckedInMarkupMacroReference();
        const string source = """
            class Greeting {
                var Name: string = ""
            }

            class Gallery {
                func Render() => Markup! {
                    <Greeting Name="Raven" />
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

        var attributeToken = Assert.Single(
            compilation.GetMacroTokens(invocation),
            static candidate => candidate.Text == "Name");
        var property = Assert.IsAssignableFrom<IPropertySymbol>(attributeToken.Symbol);
        Assert.Equal("Name", property.Name);
        Assert.Equal("Greeting", property.ContainingType?.Name);
        Assert.Contains(property.Locations, static location => location.IsInSource);
    }

    [Fact]
    public void CheckedInMarkupMacro_SupportsQualifiedComponentNames()
    {
        var macroReference = CreateCheckedInMarkupMacroReference();
        const string source = """
            namespace sample.Components {
                class StatusBadge {
                    var Label: string = ""
                }
            }

            class Dashboard {
                func Render() => Markup! {
                    <sample.Components.StatusBadge Label="Ready" />
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "html-qualified-component.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var expansion = compilation.GetSemanticModel(syntaxTree).GetMacroExpansion(invocation);
        var expansionText = Assert.IsAssignableFrom<ExpressionSyntax>(expansion?.Expression).ToString();
        Assert.Contains("OpenComponent<sample.Components.StatusBadge>", expansionText, StringComparison.Ordinal);

        var componentToken = Assert.Single(
            compilation.GetMacroTokens(invocation),
            static candidate => candidate.Text == "StatusBadge");
        var component = Assert.IsAssignableFrom<INamedTypeSymbol>(componentToken.Symbol);
        Assert.Equal("StatusBadge", component.Name);
        Assert.Equal("Components", component.ContainingNamespace?.Name);

        var attributeToken = Assert.Single(
            compilation.GetMacroTokens(invocation),
            static candidate => candidate.Text == "Label");
        var property = Assert.IsAssignableFrom<IPropertySymbol>(attributeToken.Symbol);
        Assert.Equal("Label", property.Name);
        Assert.Same(component, property.ContainingType);

        const string invalidSource = """
            let view = Markup! {
                <sample..Components.StatusBadge />
            }
            """;
        var invalidTree = SyntaxTree.ParseText(invalidSource, path: "html-invalid-qualified-component.rvn");
        var invalidCompilation = CreateConsumerCompilation(invalidTree, macroReference);
        var invalidInvocation = invalidTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var invalidExpansion = invalidCompilation.GetSemanticModel(invalidTree)
            .GetMacroExpansion(invalidInvocation);
        var diagnostic = Assert.Single(invalidExpansion?.MacroDiagnostics ?? []);
        Assert.Equal("HTML001", diagnostic.Code);
        Assert.Contains("empty segments", diagnostic.Message, StringComparison.Ordinal);
    }

    [Fact]
    public void CheckedInMarkupMacro_AppliesBuildProvidedCssScopeToElements()
    {
        var macroReference = CreateCheckedInMarkupMacroReference();
        var sourcePath = Path.GetFullPath("scoped-component.rvn");
        var featureKey = $"html.blazor.css-scope:{sourcePath}";
        var parseOptions = new ParseOptions
        {
            Features = new Dictionary<string, string>
            {
                [featureKey] = "b-scoped-component"
            }
        };
        const string source = """
            class ScopedComponent {
                func Render() => Markup! {
                    <section><span>Scoped</span></section>
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, parseOptions, path: sourcePath);
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var expansion = compilation.GetSemanticModel(syntaxTree).GetMacroExpansion(invocation);
        var expansionText = Assert.IsAssignableFrom<ExpressionSyntax>(expansion?.Expression).ToString();

        Assert.Equal(2, CountOccurrences(expansionText, "\"b-scoped-component\", \"\""));
    }

    [Fact]
    public void CheckedInMarkupMacro_MapsEmbeddedExpressionToAuthoredSourceOrigin()
    {
        var macroReference = CreateCheckedInMarkupMacroReference();
        var sourcePath = Path.GetFullPath("html-debug-origin.rvn");
        const string source = """
            class DebugView {
                val count: int = 41

                func Render() => Markup! {
                    <p>{count + 1}</p>
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: sourcePath);
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference);
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var expansion = compilation.GetSemanticModel(syntaxTree).GetMacroExpansion(invocation);

        var mappedExpression = Assert.Single(
            expansion!.Expression!.DescendantNodesAndSelf(),
            node => MacroSyntaxOrigin.TryGetSourceSpan(node, compilation, out _, out var span) &&
                source.Substring(span.Start, span.Length) == "count + 1");
        Assert.True(MacroSyntaxOrigin.TryGetSourceSpan(
            mappedExpression,
            compilation,
            out var mappedTree,
            out var mappedSpan));
        Assert.Same(syntaxTree, mappedTree);
        Assert.Equal("count + 1", source.Substring(mappedSpan.Start, mappedSpan.Length));
    }

    [Fact]
    public void CheckedInMarkupMacro_LowersComponentEventCallbacksFromReferencesAndInlineLambdas()
    {
        var macroReference = CreateCheckedInMarkupMacroReference();
        const string source = """
            import Microsoft.AspNetCore.Components.*

            class CallbackComponent {
                var Referenced: EventCallback = default(EventCallback)
                var Inline: EventCallback = default(EventCallback)
                var Generic: EventCallback<int> = default(EventCallback<int>)
            }

            class CallbackHost {
                func callback() { }

                func Render() => Markup! {
                    <CallbackComponent
                        Referenced={callback}
                        Inline={() => callback()}
                        Generic={(value) => callback()} />
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "html-component-callbacks.rvn");
        var compilation = CreateConsumerCompilation(syntaxTree, macroReference)
            .AddReferences(CreateAspNetCoreComponentsReference());
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var expansion = compilation.GetSemanticModel(syntaxTree).GetMacroExpansion(invocation);
        var expansionText = Assert.IsAssignableFrom<ExpressionSyntax>(expansion?.Expression).ToString();

        Assert.Equal(2, CountOccurrences(expansionText, ": System.Action ="));
        Assert.Equal(1, CountOccurrences(expansionText, ": System.Action<"));
        Assert.Contains("= callback;", expansionText, StringComparison.Ordinal);
        Assert.Contains("= () => callback();", expansionText, StringComparison.Ordinal);
        Assert.Contains("= (value) => callback();", expansionText, StringComparison.Ordinal);
        Assert.Equal(
            2,
            CountOccurrences(
                expansionText,
                "Microsoft.AspNetCore.Components.EventCallback.Factory.Create(self,"));
        Assert.Equal(
            1,
            CountOccurrences(
                expansionText,
                "Microsoft.AspNetCore.Components.EventCallback.Factory.Create<"));

        var genericRegion = Assert.Single(
            compilation.GetMacroFragmentRegions(invocation),
            region => source.Substring(region.Span.Start, region.Span.Length)
                .Contains("(value)", StringComparison.Ordinal));
        var targetType = Assert.IsAssignableFrom<INamedTypeSymbol>(genericRegion.TargetType);
        Assert.Equal("Action", targetType.Name);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(targetType.TypeArguments).SpecialType);

        var valueInfo = compilation.GetMacroFragmentSemanticInfo(
            invocation,
            source.LastIndexOf("(value)", StringComparison.Ordinal) + 2);
        var lambda = Assert.IsAssignableFrom<ILambdaSymbol>(valueInfo?.SymbolInfo.Symbol);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(lambda.Parameters).Type.SpecialType);
    }

    [Fact]
    public void CheckedInMarkupMacro_ResolvesSymbolsInNestedComprehensionTemplate()
    {
        var macroReference = CreateCheckedInMarkupMacroReference();
        const string source = """
            class Todo {
                val Title: string => "Build Raven"
            }

            class TodoItem {
                var Title: string = ""
            }

            class TodoList {
                val todos = [Todo()]

                func Render() => Markup! {
                    <ul>
                        {[for todo in todos if todo.Title.Length > 0 =>
                            Markup! {
                                <TodoItem Title={todo.Title} />
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
        var componentStart = source.LastIndexOf("<TodoItem", StringComparison.Ordinal);
        var componentAttributePosition = source.IndexOf("Title", componentStart, StringComparison.Ordinal) + 1;

        var todosInfo = compilation.GetMacroFragmentSemanticInfo(invocation, todosPosition);
        var todoInfo = compilation.GetMacroFragmentSemanticInfo(invocation, todoPosition);
        var titleInfo = compilation.GetMacroFragmentSemanticInfo(invocation, titlePosition);
        var nestedTodoInfo = compilation.GetMacroFragmentSemanticInfo(invocation, nestedTodoPosition);
        var nestedTitleInfo = compilation.GetMacroFragmentSemanticInfo(invocation, nestedTitlePosition);
        var componentAttributeInfo = compilation.GetSemanticModel(syntaxTree)
            .GetMacroTokenInfo(invocation, componentAttributePosition);

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

        var componentAttribute = Assert.IsAssignableFrom<IPropertySymbol>(componentAttributeInfo?.Symbol);
        Assert.Equal("Title", componentAttribute.Name);
        Assert.Equal("TodoItem", componentAttribute.ContainingType?.Name);
    }

    private static Compilation CreateConsumerCompilation(
        SyntaxTree tree,
        MacroReference macroReference)
        => Compilation.Create(
                $"MarkupMacroConsumer_{Guid.NewGuid():N}",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(macroReference);

    private static MetadataReference CreateAspNetCoreComponentsReference()
    {
        var referenceDirectory = ReferenceAssemblyPaths.GetReferenceAssemblyDir(
            targetFramework: "net10.0",
            packId: "Microsoft.AspNetCore.App.Ref");
        Assert.False(string.IsNullOrWhiteSpace(referenceDirectory));
        var referencePath = Path.Combine(referenceDirectory!, "Microsoft.AspNetCore.Components.dll");
        Assert.True(File.Exists(referencePath), $"Missing ASP.NET Core reference assembly '{referencePath}'.");
        return MetadataReference.CreateFromFile(referencePath);
    }

    private static int CountOccurrences(string text, string value)
    {
        var count = 0;
        var position = 0;
        while ((position = text.IndexOf(value, position, StringComparison.Ordinal)) >= 0)
        {
            count++;
            position += value.Length;
        }

        return count;
    }

    private static MacroReference CreateCheckedInMarkupMacroReference()
        => CreateCheckedInMacroReference("MarkupMacro.rvn");

    private static MacroReference CreateCheckedInBlazorMacroReference()
        => CreateCheckedInMacroReference("MarkupMacro.rvn", "ComponentDeclarationMacro.rvn");

    private static MacroReference CreateCheckedInMacroReference(params string[] fileNames)
    {
        var repositoryRoot = Path.GetFullPath(
            Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        var macroTrees = fileNames.Select(fileName =>
        {
            var sourcePath = Path.Combine(
                repositoryRoot,
                "samples",
                "projects",
                "macro-html-blazor",
                "macros",
                fileName);
            return SyntaxTree.ParseText(File.ReadAllText(sourcePath), path: sourcePath);
        });
        var codeAnalysisReference = MetadataReference.CreateFromFile(
            typeof(IMacroDefinition).Assembly.Location);
        var macroCompilation = Compilation.Create(
                $"CheckedInMarkupMacro_{Guid.NewGuid():N}",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(macroTrees.ToArray())
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
            display: "checked-in Blazor markup macro sample");
    }
}
