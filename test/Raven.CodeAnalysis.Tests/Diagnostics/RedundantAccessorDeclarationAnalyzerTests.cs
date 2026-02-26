using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class RedundantAccessorDeclarationAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void ValProperty_WithOnlyGetAccessor_ReportsDiagnostic()
    {
        const string code = """
class C {
    public val Name: string { get; }
}
""";

        var verifier = CreateAnalyzerVerifier<RedundantAccessorDeclarationAnalyzer>(
            code,
            [
                new DiagnosticResult(RedundantAccessorDeclarationAnalyzer.DiagnosticId)
                    .WithLocation(2, 29)
                    .WithArguments("Name")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarProperty_WithDefaultGetSetAccessors_ReportsDiagnostic()
    {
        const string code = """
class C {
    public var Count: int { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<RedundantAccessorDeclarationAnalyzer>(
            code,
            [
                new DiagnosticResult(RedundantAccessorDeclarationAnalyzer.DiagnosticId)
                    .WithLocation(2, 27)
                    .WithArguments("Count")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void Indexer_WithDefaultGetSetAccessors_ReportsDiagnostic()
    {
        const string code = """
class C {
    public var self[index: int]: int { get; set; }
}
""";

        var verifier = CreateAnalyzerVerifier<RedundantAccessorDeclarationAnalyzer>(
            code,
            [
                new DiagnosticResult(RedundantAccessorDeclarationAnalyzer.DiagnosticId)
                    .WithLocation(2, 38)
                    .WithArguments("indexer")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void AccessorList_WithInitAccessor_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public val Name: string { get; init; }
}
""";

        var verifier = CreateAnalyzerVerifier<RedundantAccessorDeclarationAnalyzer>(
            code,
            [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);
        verifier.Verify();
    }

    [Fact]
    public void AccessorList_WithAccessorModifier_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public val Name: string { get; private set; }
}
""";

        var verifier = CreateAnalyzerVerifier<RedundantAccessorDeclarationAnalyzer>(
            code,
            [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);
        verifier.Verify();
    }
}
