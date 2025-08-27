using System.Linq;
using System.Text;

using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Testing;

public class AnalyzerVerifier<TAnalyzer> where TAnalyzer : DiagnosticAnalyzer, new()
{
    public Test Test { get; set; } = new();

    public DiagnosticVerifierResult GetResult()
    {
        var workspace = RavenWorkspace.Create(targetFramework: TestTargetFramework.Default);
        var projectId = workspace.AddProject("Test");
        var docId = DocumentId.CreateNew(projectId);
        var solution = workspace.CurrentSolution.AddDocument(docId, "test.rav", SourceText.From(Test.TestCode));
        workspace.TryApplyChanges(solution);

        var project = workspace.CurrentSolution.GetProject(projectId)!;
        project = project.AddAnalyzerReference(new AnalyzerReference(new TAnalyzer()));
        foreach (var reference in ReferenceAssemblies.Default)
            project = project.AddMetadataReference(reference);
        workspace.TryApplyChanges(project.Solution);

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
                expected.Arguments.Select(x => x.ToString()).SequenceEqual(diagnostic.GetMessageArgs().Select(x => x.ToString())) &&
                expected.Location.Span.StartLinePosition == lineSpan.StartLinePosition);

            if (isExpected)
                matchedDiagnostics.Add(diagnostic);
            else
                unexpectedDiagnostics.Add(diagnostic);
        }

        foreach (var expected in expectedDiagnostics)
        {
            var isFound = actualDiagnostics.Any(actual =>
            {
                var expectedSpan = expected.Location.Span;
                return actual.Descriptor.Id == expected.Id &&
                       actual.GetMessageArgs().Select(x => x.ToString()).SequenceEqual(expected.Arguments.Select(x => x.ToString())) &&
                       actual.Location.GetLineSpan().StartLinePosition == expectedSpan.StartLinePosition;
            });

            if (!isFound)
                missingDiagnostics.Add(expected);
        }

        var result = new DiagnosticVerifierResult
        {
            Compilation = compilation,
            MatchedDiagnostics = matchedDiagnostics,
            UnexpectedDiagnostics = unexpectedDiagnostics,
            MissingDiagnostics = missingDiagnostics
        };

        return result;
    }

    public void Verify()
    {
        var result = GetResult();

        if (result.UnexpectedDiagnostics.Any() || result.MissingDiagnostics.Any())
        {
            var errorMessage = BuildErrorMessage(result.UnexpectedDiagnostics, result.MissingDiagnostics);
            throw new DiagnosticVerificationException(errorMessage, result);
        }
    }

    private static string BuildErrorMessage(List<Diagnostic> unexpected, List<DiagnosticResult> missing)
    {
        var message = new StringBuilder();
        message.AppendLine("Mismatch between expected and actual diagnostics:");

        if (unexpected.Any())
        {
            message.AppendLine("\nUnexpected Diagnostics:");
            foreach (var diag in unexpected)
            {
                var lineSpan = diag.Location.GetLineSpan();
                var line = lineSpan.StartLinePosition.Line + 1;
                var column = lineSpan.StartLinePosition.Character + 1;
                message.AppendLine($"  ({line},{column}): {diag.Descriptor.Id} - {diag.GetDescription()}");
            }
        }

        if (missing.Any())
        {
            message.AppendLine("\nMissing Expected Diagnostics:");
            foreach (var expected in missing)
            {
                var descriptor = CompilerDiagnostics.GetDescriptor(expected.Id);
                var m = descriptor is null
                    ? string.Join(",", expected.Arguments.Select(a => a?.ToString()))
                    : string.Format(descriptor.MessageFormat, expected.Arguments);

                var start = expected.Location.Span.StartLinePosition;

                message.AppendLine($"  ({start.Line + 1},{start.Character + 1}): {expected.Id} - {m}");
            }
        }

        return message.ToString();
    }
}
