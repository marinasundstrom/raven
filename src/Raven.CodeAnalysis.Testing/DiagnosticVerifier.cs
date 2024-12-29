using System;
using System.Collections.Immutable;
using System.Text;

using Castle.DynamicProxy.Generators.Emitters;

using NSubstitute;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Testing;

public class Test
{
    public string TestCode { get; set; } = string.Empty;

    //public IList<TestSource> OtherSources { get; set; } = string.Empty;

    public IList<DiagnosticResult> ExpectedDiagnostics { get; set; } = new List<DiagnosticResult>();
    public IList<string> DisabledDiagnostics { get; set; } = new List<string>();
    public TestState State { get; set; } = new TestState();
}

public class TestState
{
    public ImmutableArray<MetadataReference> AdditionalReferences { get; set; } = [];
    public ImmutableArray<MetadataReference> ReferenceAssemblies { get; set; } = Raven.CodeAnalysis.Testing.ReferenceAssemblies.Default;
}

public class DiagnosticVerifier
{
    public Test Test { get; set; } = new Test();

    public DiagnosticVerifierResult GetResult()
    {
        var syntaxTree = SyntaxTree.ParseText(Test.TestCode);
        var compilation = Compilation.Create("Test")
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([.. Test.State.ReferenceAssemblies, .. Test.State.AdditionalReferences])
            .AnalyzeCodeTemp(); // Temporary

        var actualDiagnostics = compilation.GetDiagnostics().AsEnumerable();
        var expectedDiagnostics = Test.ExpectedDiagnostics;
        var disabledDiagnostics = Test.DisabledDiagnostics.ToHashSet();

        var unexpectedDiagnostics = new List<Diagnostic>();
        var missingDiagnostics = new List<DiagnosticResult>();
        var matchedDiagnostics = new List<Diagnostic>();

        foreach (var diagnostic in actualDiagnostics)
        {
            var lineSpan = diagnostic.Location.GetLineSpan();

            // Skip disabled diagnostics
            if (disabledDiagnostics.Contains(diagnostic.Descriptor.Id))
                continue;

            // Check if the diagnostic matches any expected result
            var isExpected = expectedDiagnostics.Any(expected =>
                expected.Id == diagnostic.Descriptor.Id &&
                expected.Arguments.Select(x => x.ToString()).SequenceEqual(diagnostic.GetMessageArgs().Select(x => x.ToString())) &&
                expected.Location.Span.StartLinePosition == lineSpan.StartLinePosition);

            if (isExpected)
            {
                matchedDiagnostics.Add(diagnostic);
            }
            else
            {
                unexpectedDiagnostics.Add(diagnostic);
            }
        }

        // Check for missing expected diagnostics
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
            {
                missingDiagnostics.Add(expected);
            }
        }

        // Populate the result
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

        // Throw if there are validation errors
        if (result.UnexpectedDiagnostics.Any() || result.MissingDiagnostics.Any())
        {
            var errorMessage = BuildErrorMessage(result.UnexpectedDiagnostics, result.MissingDiagnostics);
            throw new DiagnosticVerificationException(errorMessage, result);
        }
    }

    private string BuildErrorMessage(List<Diagnostic> unexpected, List<DiagnosticResult> missing)
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
                message.AppendLine($"  ({line},{column}): {diag.Descriptor.Id} - {diag.GetMessage()}");
            }
        }

        if (missing.Any())
        {
            message.AppendLine("\nMissing Expected Diagnostics:");
            foreach (var expected in missing)
            {
                var descriptor = CompilerDiagnostics.GetDescriptor(expected.Id);
                var m = string.Format(descriptor!.MessageFormat, expected.Arguments);

                var start = expected.Location.Span.StartLinePosition;

                message.AppendLine($"  ({start.Line + 1},{start.Character + 1}): {expected.Id} - {m}");
            }
        }

        return message.ToString();
    }
}

[Serializable]
internal class DiagnosticVerificationException : Exception
{
    public DiagnosticVerificationException(string errorMessage, DiagnosticVerifierResult diagnosticResult)
     : base(errorMessage)
    {
        DiagnosticResult = diagnosticResult;
    }

    public DiagnosticVerifierResult DiagnosticResult { get; }
}

public class DiagnosticVerifierResult
{
    public List<Diagnostic> MatchedDiagnostics { get; internal set; } = new();
    public List<Diagnostic> UnexpectedDiagnostics { get; internal set; } = new();
    public List<DiagnosticResult> MissingDiagnostics { get; internal set; } = new();
    public Compilation Compilation { get; internal set; } = default!;
}

public class DiagnosticResult
{
    public string Id { get; private set; }
    public DiagnosticSeverity Severity { get; private set; }
    public DiagnosticLocation Location { get; private set; }
    public DiagnosticOptions Options { get; private set; }
    public object[] Arguments { get; private set; } = [];

    public DiagnosticResult(string id)
    {
        Id = id;
    }

    public DiagnosticResult WithSeverity(DiagnosticSeverity severity)
    {
        Severity = severity;
        return this;
    }

    public DiagnosticResult WithLocation(int line, int column)
    {
        Location = new DiagnosticLocation(new FileLinePositionSpan(string.Empty, new LinePosition(line - 1, column - 1), new LinePosition()), DiagnosticLocationOptions.None);
        return this;
    }

    public DiagnosticResult WithArguments(params object[] arguments)
    {
        Arguments = arguments;
        return this;
    }
}

public enum DiagnosticOptions
{
    None = 0,
    IgnoreAdditionalLocations = 1,
    IgnoreSeverity = 2,
}

public readonly struct DiagnosticLocation
{
    public DiagnosticLocation(FileLinePositionSpan span, DiagnosticLocationOptions options)
    {
        Span = span;
        Options = options;
    }

    public FileLinePositionSpan Span { get; }

    public DiagnosticLocationOptions Options { get; }

}

[Flags]
public enum DiagnosticLocationOptions
{
    None = 0,
    IgnoreLength = 1,
    InterpretAsMarkupKey = 2,
    UnnecessaryCode = 4,
}

public static class ReferenceAssemblies
{
    private static ImmutableArray<MetadataReference>? _default;
    private static ImmutableArray<MetadataReference>? _net9_0;

    public static ImmutableArray<MetadataReference> Default => _default ??= Net9_0;

    public static ImmutableArray<MetadataReference> Net9_0 => _net9_0 ??= GeReferences().ToImmutableArray();

    private static MetadataReference[] GeReferences()
    {
        return ReferenceAssemblyPaths.GetReferenceAssemblyPaths().Select(path => MetadataReference.CreateFromFile(path)).ToArray();
    }
}