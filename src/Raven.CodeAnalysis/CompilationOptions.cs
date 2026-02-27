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
        bool allowUnsafe = false,
        bool useRuntimeAsync = false,
        bool allowGlobalStatements = true,
        bool enableSuggestions = false,
        bool? membersPublicByDefault = null)
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
        UseRuntimeAsync = useRuntimeAsync;
        AllowGlobalStatements = allowGlobalStatements;
        EnableSuggestions = enableSuggestions;
        _membersPublicByDefault = membersPublicByDefault;
    }

    public OutputKind OutputKind { get; }

    public ImmutableDictionary<string, ReportDiagnostic> SpecificDiagnosticOptions { get; }

    public bool RunAnalyzers { get; }

    public CompilationOptions WithSpecificDiagnosticOptions(IDictionary<string, ReportDiagnostic> options)
        => new(OutputKind, SpecificDiagnosticOptions.SetItems(options), RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public CompilationOptions WithSpecificDiagnosticOption(string diagnosticId, ReportDiagnostic option)
        => new(OutputKind, SpecificDiagnosticOptions.SetItem(diagnosticId, option), RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public CompilationOptions WithRunAnalyzers(bool runAnalyzers)
        => new(OutputKind, SpecificDiagnosticOptions, runAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public PerformanceInstrumentation PerformanceInstrumentation { get; }

    public CompilationOptions WithPerformanceInstrumentation(PerformanceInstrumentation? instrumentation)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, instrumentation ?? PerformanceInstrumentation.Disabled, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public ILoweringTraceSink? LoweringTrace { get; }

    public CompilationOptions WithLoweringTrace(ILoweringTraceSink? loweringTrace)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, loweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public AsyncInvestigationOptions AsyncInvestigation { get; }

    public CompilationOptions WithAsyncInvestigation(AsyncInvestigationOptions? asyncInvestigation)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, asyncInvestigation ?? AsyncInvestigationOptions.Disabled, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public IOverloadResolutionLogger? OverloadResolutionLogger { get; }

    public CompilationOptions WithOverloadResolutionLogger(IOverloadResolutionLogger? overloadResolutionLogger)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, overloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public bool EmbedCoreTypes { get; }

    public CompilationOptions WithEmbedCoreTypes(bool embedCoreTypes)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, embedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public bool AllowUnsafe { get; }

    public CompilationOptions WithAllowUnsafe(bool allowUnsafe)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, allowUnsafe, UseRuntimeAsync, AllowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public bool UseRuntimeAsync { get; }

    public CompilationOptions WithRuntimeAsync(bool useRuntimeAsync)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, useRuntimeAsync, AllowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public bool AllowGlobalStatements { get; }

    public CompilationOptions WithAllowGlobalStatements(bool allowGlobalStatements)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, allowGlobalStatements, EnableSuggestions, _membersPublicByDefault);

    public bool EnableSuggestions { get; }

    public CompilationOptions WithEnableSuggestions(bool enableSuggestions)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, enableSuggestions, _membersPublicByDefault);

    private readonly bool? _membersPublicByDefault;

    public bool MembersPublicByDefault => _membersPublicByDefault ?? true;

    public bool MembersPublicByDefaultConfigured => _membersPublicByDefault.HasValue;

    public CompilationOptions WithMembersPublicByDefault(bool membersPublicByDefault)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, EnableSuggestions, membersPublicByDefault);
}
