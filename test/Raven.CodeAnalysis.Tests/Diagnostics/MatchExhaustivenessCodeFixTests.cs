using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class MatchExhaustivenessCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void MatchExpressionNotExhaustive_AddsMissingBooleanArm()
    {
        const string code = """
val result = match true {
    true => 1
}
""";

        const string fixedCode = """
val result = match true {
    true => 1
    false => throw System.NotImplementedException()
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, MatchExhaustivenessCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionNotExhaustive.Id).WithAnySpan().WithArguments("false")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpressionNotExhaustive_AddsTargetTypedUnionCaseArmWithPayloadPlaceholders()
    {
        const string code = """
val value: Result<int, string> = .Ok(1)

val result = match value {
    .Ok(val payload) => payload
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""";

        const string fixedCode = """
val value: Result<int, string> = .Ok(1)

val result = match value {
    .Ok(val payload) => payload
    .Error(_) => throw System.NotImplementedException()
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, MatchExhaustivenessCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionNotExhaustive.Id).WithAnySpan().WithArguments("Error")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpressionCatchAllRedundant_RemovesRedundantCatchAllArm()
    {
        const string code = """
val state: State = .On

val result = match state {
    .On => 1
    .Off => 2
    .Done => 3
    _ => 4
}

union State {
    case On
    case Off
    case Done
}
""";

        const string fixedCode = """
val state: State = .On

val result = match state {
    .On => 1
    .Off => 2
    .Done => 3
}

union State {
    case On
    case Off
    case Done
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, MatchExhaustivenessCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionCatchAllRedundant.Id).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpressionNotExhaustive_WithFormattedMessageArgument_AddsMissingNullArm()
    {
        const string code = """
func test(v: Foo) -> int {
    return match v {
        3 => 3
        int i => i
        string => 2
    }
}

union Foo(int | string?)
""";

        const string fixedCode = """
func test(v: Foo) -> int {
    return match v {
        3 => 3
        int i => i
        string => 2
        null => throw System.NotImplementedException()
    }
}

union Foo(int | string?)
""";

        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var projectId = workspace.AddProject("Test");
        var documentId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(documentId, "test.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        foreach (var reference in ReferenceAssemblies.Default)
            project = project.AddMetadataReference(reference);

        workspace.TryApplyChanges(project.Solution);

        var diagnostic = Assert.Single(workspace.GetDiagnostics(projectId)
            .Where(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive));
        var formattedDiagnostic = Diagnostic.Create(
            CompilerDiagnostics.MatchExpressionNotExhaustive,
            diagnostic.Location,
            "Missing match case: 'null'.");

        var fix = Assert.Single(workspace.GetCodeFixes(
            projectId,
            [new MatchExhaustivenessCodeFixProvider()],
            [formattedDiagnostic]));

        var updatedSolution = fix.Action.GetChangedSolution(workspace.CurrentSolution);
        var updatedCode = updatedSolution.GetDocument(documentId)!.GetTextAsync().GetAwaiter().GetResult().ToString();

        Assert.Equal(Normalize(fixedCode), Normalize(updatedCode));
        Assert.DoesNotContain("Missing match case", updatedCode, StringComparison.Ordinal);
    }

    private static string Normalize(string code)
        => code.Replace("\r\n", "\n", StringComparison.Ordinal).Trim();

    private sealed class NoOpAnalyzer : DiagnosticAnalyzer
    {
        public override void Initialize(AnalysisContext context)
        {
        }
    }
}
