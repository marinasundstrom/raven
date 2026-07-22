using System;
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
        ImmutableHashSet<string>? disabledAnalyzers = null,
        PerformanceInstrumentation? performanceInstrumentation = null,
        ILoweringTraceSink? loweringTrace = null,
        AsyncInvestigationOptions? asyncInvestigation = null,
        IOverloadResolutionLogger? overloadResolutionLogger = null,
        bool embedCoreTypes = true,
        bool allowUnsafe = false,
        bool useRuntimeAsync = false,
        bool allowGlobalStatements = true,
        bool allowNamespaceMembers = true,
        bool allowNamespaceMemberImports = true,
        bool enableSuggestions = false,
        bool? membersPublicByDefault = null,
        ReturnedValueHandlingMode? returnedValueHandlingMode = null,
        FrameworkProjectionMode frameworkProjectionMode = FrameworkProjectionMode.Standard)
    {
        OutputKind = outputKind;
        SpecificDiagnosticOptions = specificDiagnosticOptions ?? ImmutableDictionary<string, ReportDiagnostic>.Empty;
        RunAnalyzers = runAnalyzers;
        DisabledAnalyzers = disabledAnalyzers ?? ImmutableHashSet<string>.Empty.WithComparer(StringComparer.OrdinalIgnoreCase);
        PerformanceInstrumentation = performanceInstrumentation ?? PerformanceInstrumentation.Disabled;
        LoweringTrace = loweringTrace;
        AsyncInvestigation = asyncInvestigation ?? AsyncInvestigationOptions.Disabled;
        OverloadResolutionLogger = overloadResolutionLogger;
        EmbedCoreTypes = embedCoreTypes;
        AllowUnsafe = allowUnsafe;
        UseRuntimeAsync = useRuntimeAsync;
        AllowGlobalStatements = allowGlobalStatements;
        AllowNamespaceMembers = allowNamespaceMembers;
        AllowNamespaceMemberImports = allowNamespaceMemberImports;
        EnableSuggestions = enableSuggestions;
        _membersPublicByDefault = membersPublicByDefault;
        _returnedValueHandlingMode = returnedValueHandlingMode;
        FrameworkProjectionMode = frameworkProjectionMode;
    }

    public OutputKind OutputKind { get; }

    public ImmutableDictionary<string, ReportDiagnostic> SpecificDiagnosticOptions { get; }

    public bool RunAnalyzers { get; }

    public ImmutableHashSet<string> DisabledAnalyzers { get; }

    public CompilationOptions WithSpecificDiagnosticOptions(IDictionary<string, ReportDiagnostic> options)
        => new(OutputKind, SpecificDiagnosticOptions.SetItems(options), RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public CompilationOptions WithExactSpecificDiagnosticOptions(ImmutableDictionary<string, ReportDiagnostic> options)
        => new(OutputKind, options, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public CompilationOptions WithSpecificDiagnosticOption(string diagnosticId, ReportDiagnostic option)
        => new(OutputKind, SpecificDiagnosticOptions.SetItem(diagnosticId, option), RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public CompilationOptions WithRunAnalyzers(bool runAnalyzers)
        => new(OutputKind, SpecificDiagnosticOptions, runAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public CompilationOptions WithDisabledAnalyzers(IEnumerable<string> disabledAnalyzers)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, AnalyzerOptionUtilities.ParseAnalyzerNameSet(disabledAnalyzers), PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public PerformanceInstrumentation PerformanceInstrumentation { get; }

    public CompilationOptions WithPerformanceInstrumentation(PerformanceInstrumentation? instrumentation)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, instrumentation ?? PerformanceInstrumentation.Disabled, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public ILoweringTraceSink? LoweringTrace { get; }

    public CompilationOptions WithLoweringTrace(ILoweringTraceSink? loweringTrace)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, loweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public AsyncInvestigationOptions AsyncInvestigation { get; }

    public CompilationOptions WithAsyncInvestigation(AsyncInvestigationOptions? asyncInvestigation)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, asyncInvestigation ?? AsyncInvestigationOptions.Disabled, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public IOverloadResolutionLogger? OverloadResolutionLogger { get; }

    public CompilationOptions WithOverloadResolutionLogger(IOverloadResolutionLogger? overloadResolutionLogger)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, overloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public bool EmbedCoreTypes { get; }

    public CompilationOptions WithEmbedCoreTypes(bool embedCoreTypes)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, embedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public bool AllowUnsafe { get; }

    public CompilationOptions WithAllowUnsafe(bool allowUnsafe)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, allowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public bool UseRuntimeAsync { get; }

    public CompilationOptions WithRuntimeAsync(bool useRuntimeAsync)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, useRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public bool AllowGlobalStatements { get; }

    public CompilationOptions WithAllowGlobalStatements(bool allowGlobalStatements)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, allowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public bool AllowNamespaceMembers { get; }

    public CompilationOptions WithAllowNamespaceMembers(bool allowNamespaceMembers)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, allowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public bool AllowNamespaceMemberImports { get; }

    public CompilationOptions WithAllowNamespaceMemberImports(bool allowNamespaceMemberImports)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, allowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    public bool EnableSuggestions { get; }

    public CompilationOptions WithEnableSuggestions(bool enableSuggestions)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, enableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    private readonly bool? _membersPublicByDefault;

    public bool MembersPublicByDefault => _membersPublicByDefault ?? true;

    public bool MembersPublicByDefaultConfigured => _membersPublicByDefault.HasValue;

    public CompilationOptions WithMembersPublicByDefault(bool membersPublicByDefault)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, membersPublicByDefault, _returnedValueHandlingMode, FrameworkProjectionMode);

    private readonly ReturnedValueHandlingMode? _returnedValueHandlingMode;

    public ReturnedValueHandlingMode ReturnedValueHandlingMode => _returnedValueHandlingMode ?? ReturnedValueHandlingMode.Off;

    public bool ReturnedValueHandlingModeConfigured => _returnedValueHandlingMode.HasValue;

    public CompilationOptions WithReturnedValueHandlingMode(ReturnedValueHandlingMode mode)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, mode, FrameworkProjectionMode);

    public FrameworkProjectionMode FrameworkProjectionMode { get; }

    public CompilationOptions WithFrameworkProjectionMode(FrameworkProjectionMode mode)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _membersPublicByDefault, _returnedValueHandlingMode, mode);
}

public enum FrameworkProjectionMode
{
    None,
    Standard,
}
