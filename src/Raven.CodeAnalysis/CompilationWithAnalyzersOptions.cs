namespace Raven.CodeAnalysis;

public class CompilationWithAnalyzersOptions
{
    public CompilationWithAnalyzersOptions(
        bool reportSuppressedDiagnostics = false,
        bool logAnalyzerExecutionTime = false,
        bool concurrentAnalysis = true)
    {
        ReportSuppressedDiagnostics = reportSuppressedDiagnostics;
        LogAnalyzerExecutionTime = logAnalyzerExecutionTime;
        ConcurrentAnalysis = concurrentAnalysis;
    }

    public bool ReportSuppressedDiagnostics { get; }

    public bool LogAnalyzerExecutionTime { get; }

    public bool ConcurrentAnalysis { get; }

    public CompilationWithAnalyzersOptions WithReportSuppressedDiagnostics(bool value)
        => new(value, LogAnalyzerExecutionTime, ConcurrentAnalysis);

    public CompilationWithAnalyzersOptions WithLogAnalyzerExecutionTime(bool value)
        => new(ReportSuppressedDiagnostics, value, ConcurrentAnalysis);

    public CompilationWithAnalyzersOptions WithConcurrentAnalysis(bool value)
        => new(ReportSuppressedDiagnostics, LogAnalyzerExecutionTime, value);
}
