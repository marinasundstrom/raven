using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class EmitResultContractTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void DefaultDiagnosticsBecomeAnInitializedEmptyArray(bool success)
    {
        var result = new EmitResult(success, default);
        Assert.Equal(success, result.Success);
        Assert.Equal(ImmutableArray<Diagnostic>.Empty, result.Diagnostics);
    }

    [Fact]
    public void SuppliedDiagnosticsAndSuccessFlagAreRetainedIndependently()
    {
        var descriptor = DiagnosticDescriptor.Create("EMIT001", "Backend", "", "", "Backend failed",
            "Test", DiagnosticSeverity.Error, true);
        var diagnostic = Diagnostic.Create(descriptor, Location.None);
        ImmutableArray<Diagnostic> diagnostics = [diagnostic];

        var result = new EmitResult(true, diagnostics);

        Assert.True(result.Success);
        Assert.Equal(diagnostics, result.Diagnostics);
        Assert.Same(diagnostic, Assert.Single(result.Diagnostics));
    }
}
