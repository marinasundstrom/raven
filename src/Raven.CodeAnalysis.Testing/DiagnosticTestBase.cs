namespace Raven.CodeAnalysis.Testing;

public abstract class DiagnosticTestBase
{
    protected DiagnosticVerifier CreateVerifier(string testCode, IEnumerable<DiagnosticResult>? expectedDiagnostics = null, IEnumerable<string>? disabledDiagnostics = null)
    {
        var disabled = disabledDiagnostics?.ToList() ?? [];
        disabled.Add("RAV1011");

        return new DiagnosticVerifier
        {
            Test = new Test
            {
                TestCode = testCode,
                ExpectedDiagnostics = expectedDiagnostics?.ToList() ?? [],
                DisabledDiagnostics = disabled,
                /*
                State = new TestState
                {
                    var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
                    ReferenceAssemblies = [.. TargetFrameworkResolver.GetReferenceAssemblies(version).Select(x => MetadataReference.CreateFromFile(x))],
                }
                */
            }
        };
    }
}
