using System.Text;

using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Testing;

public class CodeFixTest : Test
{
    public string FixedCode { get; set; } = string.Empty;
    public int? ExpectedAppliedFixCount { get; set; }
    public int MaxIterations { get; set; } = 100;
}

public class CodeFixVerifier<TAnalyzer, TCodeFixProvider>
    where TAnalyzer : DiagnosticAnalyzer, new()
    where TCodeFixProvider : CodeFixProvider, new()
{
    public CodeFixTest Test { get; set; } = new();

    public CodeFixVerifierResult GetResult()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var projectId = workspace.AddProject("Test");
        var docId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From(Test.TestCode));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new TAnalyzer()));
        foreach (var reference in Test.State.ReferenceAssemblies)
            project = project.AddMetadataReference(reference);
        foreach (var reference in Test.State.AdditionalReferences)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

        var initialDiagnosticsResult = BuildDiagnosticResult(workspace, projectId);
        var applyResult = workspace.ApplyCodeFixes(
            projectId,
            [new TCodeFixProvider()],
            maxIterations: Test.MaxIterations);

        workspace.TryApplyChanges(applyResult.Solution);
        var updatedDocument = workspace.CurrentSolution.GetDocument(docId)!;
        var updatedCode = updatedDocument.GetTextAsync().GetAwaiter().GetResult().ToString();

        return new CodeFixVerifierResult
        {
            DiagnosticResult = initialDiagnosticsResult,
            UpdatedCode = updatedCode,
            AppliedFixCount = applyResult.AppliedFixCount,
            AppliedFixes = applyResult.AppliedFixes
        };
    }

    public void Verify()
    {
        var result = GetResult();
        if (!IsExpectedCode(result.UpdatedCode) ||
            (Test.ExpectedAppliedFixCount is { } expectedCount && result.AppliedFixCount != expectedCount) ||
            result.DiagnosticResult.UnexpectedDiagnostics.Any() ||
            result.DiagnosticResult.MissingDiagnostics.Any())
        {
            var message = BuildErrorMessage(result);
            throw new CodeFixVerificationException(message, result);
        }
    }

    private DiagnosticVerifierResult BuildDiagnosticResult(RavenWorkspace workspace, ProjectId projectId)
    {
        var compilation = workspace.GetCompilation(projectId);
        var actualDiagnostics = workspace.GetDiagnostics(projectId);
        var expectedDiagnostics = Test.ExpectedDiagnostics;
        var disabledDiagnostics = Test.DisabledDiagnostics.ToHashSet();

        var unexpectedDiagnostics = new List<Diagnostic>();
        var missingDiagnostics = new List<DiagnosticResult>();
        var matchedDiagnostics = new List<Diagnostic>();

        foreach (var diagnostic in actualDiagnostics)
        {
            var lineSpan = diagnostic.Location.GetLineSpan();

            if (disabledDiagnostics.Contains(diagnostic.Descriptor.Id))
                continue;

            var isExpected = expectedDiagnostics.Any(expected =>
                expected.Id == diagnostic.Descriptor.Id &&
                (expected.Arguments.Length == 0 ||
                 expected.Arguments.Select(x => x.ToString()).SequenceEqual(diagnostic.GetMessageArgs().Select(x => x.ToString()))) &&
                LocationMatches(expected.Location, lineSpan));

            if (isExpected)
                matchedDiagnostics.Add(diagnostic);
            else
                unexpectedDiagnostics.Add(diagnostic);
        }

        foreach (var expected in expectedDiagnostics)
        {
            var isFound = actualDiagnostics.Any(actual =>
            {
                var span = actual.Location.GetLineSpan();

                return actual.Descriptor.Id == expected.Id &&
                       (expected.Arguments.Length == 0 ||
                        actual.GetMessageArgs().Select(x => x.ToString()).SequenceEqual(expected.Arguments.Select(x => x.ToString()))) &&
                       LocationMatches(expected.Location, span);
            });

            if (!isFound)
                missingDiagnostics.Add(expected);
        }

        return new DiagnosticVerifierResult
        {
            Compilation = compilation,
            MatchedDiagnostics = matchedDiagnostics,
            UnexpectedDiagnostics = unexpectedDiagnostics,
            MissingDiagnostics = missingDiagnostics
        };
    }

    private bool IsExpectedCode(string updatedCode)
    {
        if (string.IsNullOrWhiteSpace(Test.FixedCode))
            return true;

        return NormalizeCode(updatedCode) == NormalizeCode(Test.FixedCode);
    }

    private string BuildErrorMessage(CodeFixVerifierResult result)
    {
        var sb = new StringBuilder();
        sb.AppendLine("Code fix verification failed.");

        if (result.DiagnosticResult.UnexpectedDiagnostics.Any() || result.DiagnosticResult.MissingDiagnostics.Any())
        {
            sb.AppendLine();
            sb.AppendLine("Diagnostic mismatches:");

            if (result.DiagnosticResult.UnexpectedDiagnostics.Any())
            {
                sb.AppendLine("Unexpected Diagnostics:");
                foreach (var diag in result.DiagnosticResult.UnexpectedDiagnostics)
                {
                    var lineSpan = diag.Location.GetLineSpan();
                    var line = lineSpan.StartLinePosition.Line + 1;
                    var column = lineSpan.StartLinePosition.Character + 1;
                    var endLine = lineSpan.EndLinePosition.Line + 1;
                    var endColumn = lineSpan.EndLinePosition.Character + 1;
                    sb.AppendLine($"  ({line},{column} - {endLine},{endColumn}): {diag.Descriptor.Id} - {diag.GetDescription()}");
                }
            }

            if (result.DiagnosticResult.MissingDiagnostics.Any())
            {
                sb.AppendLine("Missing Expected Diagnostics:");
                foreach (var expected in result.DiagnosticResult.MissingDiagnostics)
                {
                    var descriptor = CompilerDiagnostics.GetDescriptor(expected.Id);
                    var format = descriptor?.MessageFormat.ToString() ?? string.Empty;
                    string message;
                    try
                    {
                        message = string.Format(format, expected.Arguments);
                    }
                    catch (FormatException)
                    {
                        message = format;
                    }

                    var start = expected.Location.Span.StartLinePosition;
                    var end = expected.Location.Span.EndLinePosition;
                    var loc = expected.Location.Options.HasFlag(DiagnosticLocationOptions.IgnoreLocation)
                        ? "?:? - ?:?"
                        : $"{start.Line + 1},{start.Character + 1} - {end.Line + 1},{end.Character + 1}";

                    sb.AppendLine($"  ({loc}): {expected.Id} - {message}");
                }
            }
        }

        if (Test.ExpectedAppliedFixCount is { } expectedCount && expectedCount != result.AppliedFixCount)
        {
            sb.AppendLine();
            sb.AppendLine($"Expected applied fix count: {expectedCount}");
            sb.AppendLine($"Actual applied fix count:   {result.AppliedFixCount}");
        }

        if (!string.IsNullOrWhiteSpace(Test.FixedCode) && !IsExpectedCode(result.UpdatedCode))
        {
            sb.AppendLine();
            sb.AppendLine("Expected fixed code:");
            sb.AppendLine(NormalizeCode(Test.FixedCode));
            sb.AppendLine();
            sb.AppendLine("Actual fixed code:");
            sb.AppendLine(NormalizeCode(result.UpdatedCode));
        }

        return sb.ToString();
    }

    private static bool LocationMatches(DiagnosticLocation expected, FileLinePositionSpan actual)
    {
        if (expected.Options.HasFlag(DiagnosticLocationOptions.IgnoreLocation))
            return true;

        if (expected.Options.HasFlag(DiagnosticLocationOptions.IgnoreLength))
            return actual.StartLinePosition == expected.Span.StartLinePosition;

        return actual.StartLinePosition == expected.Span.StartLinePosition &&
               actual.EndLinePosition == expected.Span.EndLinePosition;
    }

    private static string NormalizeCode(string code)
    {
        return code.Replace("\r\n", "\n", StringComparison.Ordinal).Trim();
    }
}

public sealed class CodeFixVerifierResult
{
    public DiagnosticVerifierResult DiagnosticResult { get; internal set; } = new();
    public int AppliedFixCount { get; internal set; }
    public string UpdatedCode { get; internal set; } = string.Empty;
    public IReadOnlyList<CodeFix> AppliedFixes { get; internal set; } = [];
}

[Serializable]
public sealed class CodeFixVerificationException : Exception
{
    public CodeFixVerificationException(string errorMessage, CodeFixVerifierResult result)
        : base(errorMessage)
    {
        Result = result;
    }

    public CodeFixVerifierResult Result { get; }
}
