namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class CompilerDiagnosticsLookupTests
{
    [Fact]
    public void IsDiagnosticDefined_ReturnsTrueForKnownId()
    {
        CompilerDiagnostics.IsDiagnosticDefined("RAV1000").ShouldBeTrue();
    }

    [Fact]
    public void IsDiagnosticDefined_ReturnsFalseForUnknownOrNullId()
    {
        CompilerDiagnostics.IsDiagnosticDefined("RAV9999").ShouldBeFalse();
        CompilerDiagnostics.IsDiagnosticDefined(null!).ShouldBeFalse();
    }

    [Fact]
    public void AllDiagnosticIds_MatchesAllDescriptors()
    {
        var allDescriptors = CompilerDiagnostics.AllDescriptors;
        var allIds = CompilerDiagnostics.AllDiagnosticIds;

        allIds.Count.ShouldBe(allDescriptors.Length);

        foreach (var descriptor in allDescriptors)
        {
            allIds.Contains(descriptor.Id).ShouldBeTrue();
            ReferenceEquals(descriptor.Id, string.Intern(descriptor.Id)).ShouldBeTrue();
        }
    }
}
