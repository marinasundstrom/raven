using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class NonNullDeclarationsCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesNullableTypeToOption()
    {
        var code = """
func Test() {
    var value: int? = null
}
""";

        var fixedCode = """
func Test() {
    var value: Option<int> = null
}
""";

        var verifier = CreateCodeFixVerifier<NonNullDeclarationsAnalyzer, PreferOptionOverNullableCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(NonNullDeclarationsAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void RegistersRewriteToUseOptionFix_ForSimpleNullableFlow()
    {
        var code = """
func Test() {
    val text: string? = GetName()
    if text != null {
        Console.WriteLine(text)
    }
}
""";

        var fixes = GetAvailableCodeFixes(code);

        Assert.Contains(fixes, fix => fix.Action.Title == "Use 'Option<string>'");
        Assert.Contains(fixes, fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");
    }

    [Fact]
    public void RegistersOnlyRewriteToUseOptionFix_ForInferredNullableFlow()
    {
        var code = """
func Test() {
    val text = GetName()
    if text != null {
        Console.WriteLine(text)
    }
}

func GetName() -> string? {
    return null
}
""";

        var fixes = GetAvailableCodeFixes(code);

        Assert.DoesNotContain(fixes, fix => fix.Action.Title == "Use 'Option<string>'");
        Assert.Contains(fixes, fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");
    }

    [Fact]
    public void AppliesRewriteToUseOptionFix_ForSimpleNullableFlow()
    {
        var code = """
func Test() {
    val text: string? = GetName()
    if text != null {
        Console.WriteLine(text)
    }
}
""";

        var expected = """
func Test() {
    val maybeText: Option<string> = GetName()
    if maybeText is Some(val text) {
        Console.WriteLine(text)
    }
}
""";

        var result = GetCodeFixResult(
            code,
            fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");

        Assert.Equal(1, result.AppliedFixCount);
        Assert.Equal("Rewrite nullable flow to Option pattern matching", Assert.Single(result.AppliedFixes).Action.Title);
        Assert.Equal(Normalize(expected), Normalize(result.UpdatedCode));
    }

    [Fact]
    public void AppliesRewriteToUseOptionFix_ForInferredNullableFlow()
    {
        var code = """
func Test() {
    val text = GetName()
    if text != null {
        Console.WriteLine(text)
    }
}

func GetName() -> string? {
    return null
}
""";

        var expected = """
func Test() {
    val maybeText: Option<string> = GetName()
    if maybeText is Some(val text) {
        Console.WriteLine(text)
    }
}

func GetName() -> string? {
    return null
}
""";

        var result = GetCodeFixResult(
            code,
            fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");

        Assert.Equal(1, result.AppliedFixCount);
        Assert.Equal("Rewrite nullable flow to Option pattern matching", Assert.Single(result.AppliedFixes).Action.Title);
        Assert.Equal(Normalize(expected), Normalize(result.UpdatedCode));
    }

    [Fact]
    public void DoesNotRegisterRewriteToUseOptionFix_WhenLocalEscapesIfFlow()
    {
        var code = """
func Test() {
    val text: string? = GetName()
    if text != null {
        Console.WriteLine(text)
    }

    Console.WriteLine(text)
}
""";

        var fixes = GetAvailableCodeFixes(code);

        Assert.DoesNotContain(fixes, fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");
    }

    [Fact]
    public void AppliesRewriteToUseOptionFix_WithElseBranch_PreservesIfElse()
    {
        var code = """
func Test() {
    val text: string? = GetName()
    if text != null {
        Console.WriteLine(text)
    } else {
        Console.WriteLine("missing")
    }
}
""";

        var expected = """
func Test() {
    val maybeText: Option<string> = GetName()
    if maybeText is Some(val text) {
        Console.WriteLine(text)
    } else {
        Console.WriteLine("missing")
    }
}
""";

        var result = GetCodeFixResult(
            code,
            fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");

        Assert.Equal(1, result.AppliedFixCount);
        Assert.Equal("Rewrite nullable flow to Option pattern matching", Assert.Single(result.AppliedFixes).Action.Title);
        Assert.Equal(Normalize(expected), Normalize(result.UpdatedCode));
    }

    [Fact]
    public void AppliesRewriteToUseOptionFix_WithTypePattern_AsSomePattern()
    {
        var code = """
func Test() {
    val value: object? = GetValue()
    if value is string text {
        Console.WriteLine(text)
    }
}
""";

        var expected = """
func Test() {
    val maybeValue: Option<object> = GetValue()
    if maybeValue is Some(string text) {
        Console.WriteLine(text)
    }
}
""";

        var result = GetCodeFixResult(
            code,
            fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");

        Assert.Equal(1, result.AppliedFixCount);
        Assert.Equal("Rewrite nullable flow to Option pattern matching", Assert.Single(result.AppliedFixes).Action.Title);
        Assert.Equal(Normalize(expected), Normalize(result.UpdatedCode));
    }

    [Fact]
    public void AppliesRewriteToUseOptionFix_WithTypePatternElse_PreservesIfElse()
    {
        var code = """
func Test() {
    val value: object? = GetValue()
    if value is string text {
        Console.WriteLine(text)
    } else {
        Console.WriteLine("other")
    }
}
""";

        var expected = """
func Test() {
    val maybeValue: Option<object> = GetValue()
    if maybeValue is Some(string text) {
        Console.WriteLine(text)
    } else {
        Console.WriteLine("other")
    }
}
""";

        var result = GetCodeFixResult(
            code,
            fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");

        Assert.Equal(1, result.AppliedFixCount);
        Assert.Equal("Rewrite nullable flow to Option pattern matching", Assert.Single(result.AppliedFixes).Action.Title);
        Assert.Equal(Normalize(expected), Normalize(result.UpdatedCode));
    }

    [Fact]
    public void AppliesRewriteToUseOptionFix_WithReturningElseFlow_PreservesIfElse()
    {
        var code = """
func Test() -> string {
    val text: string? = GetName()
    if text != null {
        return text
    } else {
        return "missing"
    }
}
""";

        var expected = """
func Test() -> string {
    val maybeText: Option<string> = GetName()
    if maybeText is Some(val text) {
        return text
    } else {
        return "missing"
    }
}
""";

        var result = GetCodeFixResult(
            code,
            fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");

        Assert.Equal(1, result.AppliedFixCount);
        Assert.Equal("Rewrite nullable flow to Option pattern matching", Assert.Single(result.AppliedFixes).Action.Title);
        Assert.Equal(Normalize(expected), Normalize(result.UpdatedCode));
    }

    [Fact]
    public void AppliesRewriteToUseOptionFix_WithSameTargetAssignments_PreservesIfElse()
    {
        var code = """
func Test() -> string {
    val text: string? = GetName()
    var result = ""
    if text != null {
        result = text
    } else {
        result = "missing"
    }

    return result
}
""";

        var expected = """
func Test() -> string {
    val maybeText: Option<string> = GetName()
    var result = ""
    if maybeText is Some(val text) {
        result = text
    } else {
        result = "missing"
    }

    return result
}
""";

        var result = GetCodeFixResult(
            code,
            fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");

        Assert.Equal(1, result.AppliedFixCount);
        Assert.Equal("Rewrite nullable flow to Option pattern matching", Assert.Single(result.AppliedFixes).Action.Title);
        Assert.Equal(Normalize(expected), Normalize(result.UpdatedCode));
    }

    [Fact]
    public void AppliesRewriteToUseOptionFix_ForIsNotNullFlow()
    {
        var code = """
func Test() {
    val text: string? = GetName()
    if text is not null {
        Console.WriteLine(text)
    }
}
""";

        var expected = """
func Test() {
    val maybeText: Option<string> = GetName()
    if maybeText is Some(val text) {
        Console.WriteLine(text)
    }
}
""";

        var result = GetCodeFixResult(
            code,
            fix => fix.Action.Title == "Rewrite nullable flow to Option pattern matching");

        Assert.Equal(1, result.AppliedFixCount);
        Assert.Equal("Rewrite nullable flow to Option pattern matching", Assert.Single(result.AppliedFixes).Action.Title);
        Assert.Equal(Normalize(expected), Normalize(result.UpdatedCode));
    }

    private static IReadOnlyList<CodeFix> GetAvailableCodeFixes(string code)
    {
        var (workspace, projectId, _) = CreateWorkspace(code);
        return workspace.GetCodeFixes(projectId, [new PreferOptionOverNullableCodeFixProvider()]);
    }

    private static TestCodeFixResult GetCodeFixResult(
        string code,
        Func<CodeFix, bool>? predicate = null)
    {
        var (workspace, projectId, documentId) = CreateWorkspace(code);

        var applyResult = workspace.ApplyCodeFixes(
            projectId,
            [new PreferOptionOverNullableCodeFixProvider()],
            predicate: predicate);

        workspace.TryApplyChanges(applyResult.Solution);
        var updatedDocument = workspace.CurrentSolution.GetDocument(documentId)!;
        var updatedCode = updatedDocument.GetTextAsync().GetAwaiter().GetResult().ToString();

        return new TestCodeFixResult(updatedCode, applyResult.AppliedFixCount, applyResult.AppliedFixes);
    }

    private static string Normalize(string code)
        => code.Replace("\r\n", "\n", StringComparison.Ordinal).Trim();

    private static (RavenWorkspace Workspace, ProjectId ProjectId, DocumentId DocumentId) CreateWorkspace(string code)
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var projectId = workspace.AddProject("Test");
        var documentId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(documentId, "test.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        if (project.CompilationOptions is { } compilationOptions)
            project = project.WithCompilationOptions(compilationOptions.WithEnableSuggestions(true));

        project = project.AddAnalyzerReference(new AnalyzerReference(new NonNullDeclarationsAnalyzer()));
        foreach (var reference in ReferenceAssemblies.Default)
            project = project.AddMetadataReference(reference);

        workspace.TryApplyChanges(project.Solution);
        return (workspace, projectId, documentId);
    }

    private sealed record TestCodeFixResult(
        string UpdatedCode,
        int AppliedFixCount,
        IReadOnlyList<CodeFix> AppliedFixes);
}
