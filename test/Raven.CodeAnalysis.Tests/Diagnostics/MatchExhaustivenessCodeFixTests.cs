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
let result = match true {
    true => 1
}
""";

        const string fixedCode = """
let result = match true {
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
    public void MatchExpressionNotExhaustive_AddsTargetTypedUnionCaseArmWithPayloadBindings()
    {
        const string code = """
let value: Result<int, string> = .Ok(1)

let result = match value {
    .Ok(let payload) => payload
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""";

        const string fixedCode = """
let value: Result<int, string> = .Ok(1)

let result = match value {
    .Ok(let payload) => payload
    .Error(let message) => throw System.NotImplementedException()
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
    public void MatchExpressionNotExhaustive_AddsAllDeclaredUnionCaseArms()
    {
        const string code = """
func inspect(result: Result) -> int {
    return match result {
    }
}

union Result {
    case None
    case Success(value: int)
    case Error(message: string)
}
""";

        const string fixedCode = """
func inspect(result: Result) -> int {
    return match result {
        .Error(let message) => throw System.NotImplementedException()
        .None => throw System.NotImplementedException()
        .Success(let value) => throw System.NotImplementedException()
    }
}

union Result {
    case None
    case Success(value: int)
    case Error(message: string)
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, MatchExhaustivenessCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionNotExhaustive.Id).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpressionNotExhaustive_AddsAllSealedClassHierarchyArmsWithTypedBindings()
    {
        const string code = """
func inspect(expression: Expression) -> int {
    return match expression {
    }
}

sealed class Expression permits IdentifierExpressionSyntax, LiteralExpressionSyntax
class IdentifierExpressionSyntax : Expression
class LiteralExpressionSyntax : Expression
""";

        const string fixedCode = """
func inspect(expression: Expression) -> int {
    return match expression {
        IdentifierExpressionSyntax identifierExpression => throw System.NotImplementedException()
        LiteralExpressionSyntax literalExpression => throw System.NotImplementedException()
    }
}

sealed class Expression permits IdentifierExpressionSyntax, LiteralExpressionSyntax
class IdentifierExpressionSyntax : Expression
class LiteralExpressionSyntax : Expression
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, MatchExhaustivenessCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionNotExhaustive.Id).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpressionNotExhaustive_AddsAllSealedRecordHierarchyArmsWithPositionalBindings()
    {
        const string code = """
func inspect(node: Node) -> int {
    return match node {
    }
}

sealed record Node permits Case, Empty
record Case(No: int) : Node
record Empty : Node
""";

        const string fixedCode = """
func inspect(node: Node) -> int {
    return match node {
        Case(let no) => throw System.NotImplementedException()
        Empty => throw System.NotImplementedException()
    }
}

sealed record Node permits Case, Empty
record Case(No: int) : Node
record Empty : Node
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, MatchExhaustivenessCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionNotExhaustive.Id).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpressionNotExhaustive_AddsAllParameterizedUnionArmsWithTypedBindings()
    {
        const string code = """
func inspect(value: Value) -> int {
    return match value {
    }
}

union Value(int | string)
""";

        const string fixedCode = """
func inspect(value: Value) -> int {
    return match value {
        int v => throw System.NotImplementedException()
        string v => throw System.NotImplementedException()
    }
}

union Value(int | string)
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, MatchExhaustivenessCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionNotExhaustive.Id).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatementNotExhaustive_AddsAllEnumArms()
    {
        const string code = """
func inspect(state: State) {
    match state {
    }
}

enum State {
    None
    Some
}
""";

        const string fixedCode = """
func inspect(state: State) {
    match state {
        .None => throw System.NotImplementedException()
        .Some => throw System.NotImplementedException()
    }
}

enum State {
    None
    Some
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, MatchExhaustivenessCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.MatchExpressionNotExhaustive.Id).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpressionNotExhaustive_MultipleMissingCasesRegistersOneAddAllAction()
    {
        const string code = """
func inspect(state: State) -> int {
    return match state {
    }
}

union State {
    case None
    case Some(value: int)
    case Error(message: string)
}
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

        var diagnostics = workspace.GetDiagnostics(projectId)
            .Where(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive)
            .ToArray();
        Assert.Equal(3, diagnostics.Length);

        var fix = Assert.Single(workspace.GetCodeFixes(
            projectId,
            [new MatchExhaustivenessCodeFixProvider()],
            diagnostics));

        Assert.Equal("Add all missing match arms", fix.Action.Title);
    }

    [Fact]
    public void MatchStatementNotExhaustive_SyntaxHierarchyAddsTypedClassPatterns()
    {
        const string code = """
import Raven.CodeAnalysis.Syntax.*

func inspect(expression: ExpressionSyntax) {
    match expression {
    }
}
""";

        var updatedCode = ApplySyntaxHierarchyFix(code);

        Assert.Contains(
            "IdentifierNameSyntax identifierName => throw System.NotImplementedException()",
            updatedCode,
            StringComparison.Ordinal);
        Assert.Contains(
            "LiteralExpressionSyntax literalExpression => throw System.NotImplementedException()",
            updatedCode,
            StringComparison.Ordinal);
    }

    [Fact]
    public void MatchStatementNotExhaustive_SyntaxHierarchyInsideMacroAddsTypedClassPatterns()
    {
        const string code = """
import Raven.CodeAnalysis.Syntax.*

macro func inspect(expression: ExpressionSyntax) {
    match expression {
    }
    expand SyntaxFactory.ParseExpression("0")
}
""";

        var updatedCode = ApplySyntaxHierarchyFix(code);

        Assert.Contains(
            "IdentifierNameSyntax identifierName => throw System.NotImplementedException()",
            updatedCode,
            StringComparison.Ordinal);
        Assert.Contains(
            "LiteralExpressionSyntax literalExpression => throw System.NotImplementedException()",
            updatedCode,
            StringComparison.Ordinal);
    }

    [Fact]
    public void MatchExpressionCatchAllRedundant_RemovesRedundantCatchAllArm()
    {
        const string code = """
let state: State = .On

let result = match state {
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
let state: State = .On

let result = match state {
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

    private static string ApplySyntaxHierarchyFix(string code)
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var projectId = workspace.AddProject("Test");
        var documentId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(documentId, "test.rvn", SourceText.From(code));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        foreach (var reference in ReferenceAssemblies.Default)
            project = project.AddMetadataReference(reference);
        project = project.AddMetadataReference(
            MetadataReference.CreateFromFile(typeof(Raven.CodeAnalysis.Syntax.ExpressionSyntax).Assembly.Location));
        workspace.TryApplyChanges(project.Solution);

        var diagnostics = workspace.GetDiagnostics(projectId)
            .Where(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive)
            .ToArray();
        Assert.True(diagnostics.Length > 10);

        var fix = Assert.Single(workspace.GetCodeFixes(
            projectId,
            [new MatchExhaustivenessCodeFixProvider()],
            diagnostics));
        Assert.Equal("Add all missing match arms", fix.Action.Title);

        var updatedSolution = fix.Action.GetChangedSolution(workspace.CurrentSolution);
        return updatedSolution.GetDocument(documentId)!.GetTextAsync().GetAwaiter().GetResult().ToString();
    }

    private sealed class NoOpAnalyzer : DiagnosticAnalyzer
    {
        public override void Initialize(AnalysisContext context)
        {
        }
    }
}
