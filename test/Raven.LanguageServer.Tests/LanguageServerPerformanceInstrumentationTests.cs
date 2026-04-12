using OmniSharp.Extensions.LanguageServer.Protocol;

using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public sealed class LanguageServerPerformanceInstrumentationTests
{
    [Fact]
    public void CreateReport_IncludesFirstLookupAfterEditAndStageData()
    {
        LanguageServerPerformanceInstrumentation.Reset();

        var uri = DocumentUri.From("/workspace/test.rav");
        LanguageServerPerformanceInstrumentation.RecordDocumentEdit(uri, version: 4, source: "didChange");
        LanguageServerPerformanceInstrumentation.RecordOperation(
            "hover",
            uri,
            version: 4,
            totalMs: 42,
            stages:
            [
                new LanguageServerPerformanceInstrumentation.StageTiming("gateWait", 5),
                new LanguageServerPerformanceInstrumentation.StageTiming("analysisContext", 7),
                new LanguageServerPerformanceInstrumentation.StageTiming("semanticModel", 11)
            ]);

        var report = LanguageServerPerformanceInstrumentation.CreateReport();

        report.ShouldContain("Operation: hover");
        report.ShouldContain("FirstLookupAfterEditCount: 1");
        report.ShouldContain("FirstLookupAfterEditSources:");
        report.ShouldContain("didChange: 1");
        report.ShouldContain("gateWait:");
        report.ShouldContain("semanticModel:");
    }
}
