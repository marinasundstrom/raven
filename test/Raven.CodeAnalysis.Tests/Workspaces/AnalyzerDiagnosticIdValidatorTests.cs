namespace Raven.CodeAnalysis.Tests.Workspaces;

public class AnalyzerDiagnosticIdValidatorTests
{
    [Fact]
    public void Validate_ExternalAnalyzerWithRavPrefix_Throws()
    {
        Should.Throw<InvalidOperationException>(() =>
            AnalyzerDiagnosticIdValidator.Validate("External.TestAnalyzer", "RAV9999", isInternalAnalyzer: false));
    }

    [Fact]
    public void Validate_ExternalAnalyzerWithNonRavPrefix_DoesNotThrow()
    {
        Should.NotThrow(() =>
            AnalyzerDiagnosticIdValidator.Validate("External.TestAnalyzer", "AN0001", isInternalAnalyzer: false));
    }

    [Fact]
    public void Validate_InternalAnalyzerOverridingCompilerDiagnostic_Throws()
    {
        Should.Throw<InvalidOperationException>(() =>
            AnalyzerDiagnosticIdValidator.Validate("Raven.CodeAnalysis.Diagnostics.InternalAnalyzer", "RAV1000", isInternalAnalyzer: true));
    }

    [Fact]
    public void Validate_InternalAnalyzerWithNewDiagnosticId_DoesNotThrow()
    {
        Should.NotThrow(() =>
            AnalyzerDiagnosticIdValidator.Validate("Raven.CodeAnalysis.Diagnostics.InternalAnalyzer", "RAV9900", isInternalAnalyzer: true));
    }
}
