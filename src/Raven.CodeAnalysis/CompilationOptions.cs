using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class CompilationOptions
{
    public CompilationOptions()
        : this(OutputKind.ConsoleApplication)
    {
    }

    public CompilationOptions(
        OutputKind outputKind,
        ImmutableDictionary<string, ReportDiagnostic>? specificDiagnosticOptions = null,
        bool runAnalyzers = true,
        PerformanceInstrumentation? performanceInstrumentation = null,
        ILoweringTraceSink? loweringTrace = null,
        AsyncInvestigationOptions? asyncInvestigation = null,
        IOverloadResolutionLogger? overloadResolutionLogger = null,
        bool embedCoreTypes = true,
        bool allowUnsafe = false)
    {
        OutputKind = outputKind;
        SpecificDiagnosticOptions = specificDiagnosticOptions ?? ImmutableDictionary<string, ReportDiagnostic>.Empty;
        RunAnalyzers = runAnalyzers;
        PerformanceInstrumentation = performanceInstrumentation ?? PerformanceInstrumentation.Disabled;
        LoweringTrace = loweringTrace;
        AsyncInvestigation = asyncInvestigation ?? AsyncInvestigationOptions.Disabled;
        OverloadResolutionLogger = overloadResolutionLogger;
        EmbedCoreTypes = embedCoreTypes;
        AllowUnsafe = allowUnsafe;
    }

    public OutputKind OutputKind { get; }

    public ImmutableDictionary<string, ReportDiagnostic> SpecificDiagnosticOptions { get; }

    public bool RunAnalyzers { get; }

    public CompilationOptions WithSpecificDiagnosticOptions(IDictionary<string, ReportDiagnostic> options)
        => new(OutputKind, SpecificDiagnosticOptions.SetItems(options), RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe);

    public CompilationOptions WithSpecificDiagnosticOption(string diagnosticId, ReportDiagnostic option)
        => new(OutputKind, SpecificDiagnosticOptions.SetItem(diagnosticId, option), RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe);

    public CompilationOptions WithRunAnalyzers(bool runAnalyzers)
        => new(OutputKind, SpecificDiagnosticOptions, runAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe);

    public PerformanceInstrumentation PerformanceInstrumentation { get; }

    public CompilationOptions WithPerformanceInstrumentation(PerformanceInstrumentation? instrumentation)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, instrumentation ?? PerformanceInstrumentation.Disabled, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe);

    public ILoweringTraceSink? LoweringTrace { get; }

    public CompilationOptions WithLoweringTrace(ILoweringTraceSink? loweringTrace)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, loweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe);

    public AsyncInvestigationOptions AsyncInvestigation { get; }

    public CompilationOptions WithAsyncInvestigation(AsyncInvestigationOptions? asyncInvestigation)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, asyncInvestigation ?? AsyncInvestigationOptions.Disabled, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe);

    public IOverloadResolutionLogger? OverloadResolutionLogger { get; }

    public CompilationOptions WithOverloadResolutionLogger(IOverloadResolutionLogger? overloadResolutionLogger)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, overloadResolutionLogger, EmbedCoreTypes, AllowUnsafe);

    public bool EmbedCoreTypes { get; }

    public CompilationOptions WithEmbedCoreTypes(bool embedCoreTypes)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, embedCoreTypes, AllowUnsafe);

    public bool AllowUnsafe { get; }

    public CompilationOptions WithAllowUnsafe(bool allowUnsafe)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, allowUnsafe);
}
