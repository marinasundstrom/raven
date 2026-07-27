using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public sealed class ConditionalDirectiveTests
{
    [Fact]
    public void DefinedSymbol_SelectsIfBranchAndPreservesInactiveText()
    {
        const string source = """
#if DEBUG
func DebugOnly() {}
#else
this is deliberately not valid Raven !!!
#endif
""";
        var options = new ParseOptions().WithPreprocessorSymbols(["DEBUG"]);

        var tree = SyntaxTree.ParseText(source, options);

        Assert.Empty(tree.GetDiagnostics());
        Assert.Equal(source, tree.GetRoot().ToFullString());
        Assert.Equal(
            ["DebugOnly"],
            tree.GetRoot()
                .DescendantNodes()
                .OfType<FunctionStatementSyntax>()
                .Select(static function => function.Identifier.Text));
        Assert.Contains(
            tree.GetRoot().DescendantTrivia(),
            static trivia => trivia.Kind == SyntaxKind.DisabledTextTrivia);

        var directives = GetDirectives(tree);
        Assert.Collection(
            directives,
            directive =>
            {
                Assert.Equal(ConditionalDirectiveKind.If, directive.DirectiveKind);
                Assert.True(directive.IsBranchActive);
                Assert.True(directive.BranchTaken);
                Assert.Equal("DEBUG", directive.ConditionText);
            },
            directive =>
            {
                Assert.Equal(ConditionalDirectiveKind.Else, directive.DirectiveKind);
                Assert.False(directive.IsBranchActive);
                Assert.False(directive.BranchTaken);
            },
            directive => Assert.Equal(ConditionalDirectiveKind.EndIf, directive.DirectiveKind));
    }

    [Fact]
    public void UndefinedSymbol_SelectsElseBranch()
    {
        const string source = """
#if DEBUG
func DebugOnly() {}
#elif TRACE
func TraceOnly() {}
#else
func ReleaseOnly() {}
#endif
""";

        var tree = SyntaxTree.ParseText(source);

        Assert.Empty(tree.GetDiagnostics());
        Assert.Equal(
            ["ReleaseOnly"],
            tree.GetRoot()
                .DescendantNodes()
                .OfType<FunctionStatementSyntax>()
                .Select(static function => function.Identifier.Text));
    }

    [Fact]
    public void RavenBooleanOperatorsAndParenthesesAreSupported()
    {
        const string source = """
#if DEBUG and (WINDOWS or not TRACE)
func Selected() {}
#endif
""";
        var options = new ParseOptions().WithPreprocessorSymbols(["DEBUG", "WINDOWS"]);

        var tree = SyntaxTree.ParseText(source, options);

        Assert.Empty(tree.GetDiagnostics());
        Assert.Single(tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>());
    }

    [Fact]
    public void NestedConditionalInsideInactiveBranchIsBalanced()
    {
        const string source = """
#if OUTER
#if INNER
not Raven
#endif
#else
func Selected() {}
#endif
""";

        var tree = SyntaxTree.ParseText(source);

        Assert.Empty(tree.GetDiagnostics());
        Assert.Single(tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>());
        Assert.Equal(5, GetDirectives(tree).Length);
    }

    [Fact]
    public void MalformedConditionalStructureReportsDiagnostics()
    {
        const string source = """
#else
#if DEBUG and
func Main() {}
""";

        var diagnostics = SyntaxTree.ParseText(source).GetDiagnostics().ToArray();

        Assert.Contains(
            diagnostics,
            static diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnexpectedConditionalDirective);
        Assert.Contains(
            diagnostics,
            static diagnostic => diagnostic.Descriptor == CompilerDiagnostics.InvalidConditionalDirectiveExpression);
        Assert.Contains(
            diagnostics,
            static diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MissingEndIfDirective);
    }

    [Fact]
    public void IncrementalDirectiveEditReevaluatesWholeTree()
    {
        const string original = """
#if DEBUG
func DebugOnly() {}
#else
func ReleaseOnly() {}
#endif
""";
        const string updated = """
#if RELEASE
func DebugOnly() {}
#else
func ReleaseOnly() {}
#endif
""";
        var options = new ParseOptions().WithPreprocessorSymbols(["DEBUG"]);
        var tree = SyntaxTree.ParseText(original, options);

        var updatedTree = tree.WithChangedText(SourceText.From(updated));

        Assert.Equal(
            ["ReleaseOnly"],
            updatedTree.GetRoot()
                .DescendantNodes()
                .OfType<FunctionStatementSyntax>()
                .Select(static function => function.Identifier.Text));
    }

    [Fact]
    public async Task ProjectParseOptionChangeReparsesDocuments()
    {
        const string source = """
#if DEBUG
func DebugOnly() {}
#else
func ReleaseOnly() {}
#endif
""";
        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "Conditional",
            parseOptions: new ParseOptions().WithPreprocessorSymbols(["DEBUG"]));
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        var document = project.AddDocument("main.rvn", SourceText.From(source));
        workspace.TryApplyChanges(document.Project.Solution);

        var initialTree = await workspace.CurrentSolution.GetDocument(document.Id)!.GetSyntaxTreeAsync();
        Assert.Equal(
            "DebugOnly",
            initialTree!.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single().Identifier.Text);

        project = workspace.CurrentSolution.GetProject(projectId)!
            .WithParseOptions(new ParseOptions());
        workspace.TryApplyChanges(project.Solution);

        var updatedTree = await workspace.CurrentSolution.GetDocument(document.Id)!.GetSyntaxTreeAsync();
        Assert.Equal(
            "ReleaseOnly",
            updatedTree!.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single().Identifier.Text);
        Assert.NotSame(initialTree, updatedTree);
    }

    private static ConditionalDirectiveTriviaSyntax[] GetDirectives(SyntaxTree tree)
        => tree.GetRoot()
            .DescendantTrivia()
            .Where(static trivia => trivia.HasStructure)
            .Select(static trivia => trivia.GetStructure())
            .OfType<ConditionalDirectiveTriviaSyntax>()
            .ToArray();
}
