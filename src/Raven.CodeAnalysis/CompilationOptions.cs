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
        MetadataReferenceHostKind metadataReferenceHostKind = MetadataReferenceHostKind.Runtime)
    {
        OutputKind = outputKind;
        SpecificDiagnosticOptions = specificDiagnosticOptions ?? ImmutableDictionary<string, ReportDiagnostic>.Empty;
        RunAnalyzers = runAnalyzers;
        PerformanceInstrumentation = performanceInstrumentation ?? PerformanceInstrumentation.Disabled;
        LoweringTrace = loweringTrace;
        MetadataReferenceHostKind = metadataReferenceHostKind;
    }

    public OutputKind OutputKind { get; }

    public ImmutableDictionary<string, ReportDiagnostic> SpecificDiagnosticOptions { get; }

    public bool RunAnalyzers { get; }

    public CompilationOptions WithSpecificDiagnosticOptions(IDictionary<string, ReportDiagnostic> options)
        => new(OutputKind, SpecificDiagnosticOptions.SetItems(options), RunAnalyzers, PerformanceInstrumentation, LoweringTrace, MetadataReferenceHostKind);

    public CompilationOptions WithSpecificDiagnosticOption(string diagnosticId, ReportDiagnostic option)
        => new(OutputKind, SpecificDiagnosticOptions.SetItem(diagnosticId, option), RunAnalyzers, PerformanceInstrumentation, LoweringTrace, MetadataReferenceHostKind);

    public CompilationOptions WithRunAnalyzers(bool runAnalyzers)
        => new(OutputKind, SpecificDiagnosticOptions, runAnalyzers, PerformanceInstrumentation, LoweringTrace, MetadataReferenceHostKind);

    public PerformanceInstrumentation PerformanceInstrumentation { get; }

    public CompilationOptions WithPerformanceInstrumentation(PerformanceInstrumentation? instrumentation)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, instrumentation ?? PerformanceInstrumentation.Disabled, LoweringTrace, MetadataReferenceHostKind);

    public ILoweringTraceSink? LoweringTrace { get; }

    public CompilationOptions WithLoweringTrace(ILoweringTraceSink? loweringTrace)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, loweringTrace, MetadataReferenceHostKind);

    public MetadataReferenceHostKind MetadataReferenceHostKind { get; }

    public CompilationOptions WithMetadataReferenceHost(MetadataReferenceHostKind hostKind)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, hostKind);
}