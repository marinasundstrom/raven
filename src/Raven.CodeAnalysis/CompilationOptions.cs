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
        ReturnedValueHandlingMode? returnedValueHandlingMode = null,
        FrameworkProjectionMode frameworkProjectionMode = FrameworkProjectionMode.Standard,
        bool enableIsNotNullNarrowing = false,
        ImmutableDictionary<string, string>? externalConstantValues = null,
        ImmutableHashSet<string>? enabledAnalyzers = null,
        OptimizationLevel optimizationLevel = OptimizationLevel.Debug,
        bool synthesizeStructuralToString = true,
        ImmutableDictionary<string, bool>? generatedCodeOptions = null,
        MetadataImportOptions? metadataImportOptions = null,
        RuntimeIterationContract? runtimeIterationContract = null,
        RuntimePropagationContract? runtimePropagationContract = null,
        bool allowArrayCovariance = true)
    {
        AllowArrayCovariance = allowArrayCovariance;
        MetadataImportOptions = metadataImportOptions;
        RuntimeIterationContract = runtimeIterationContract;
        RuntimePropagationContract = runtimePropagationContract;
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
        _returnedValueHandlingMode = returnedValueHandlingMode;
        FrameworkProjectionMode = frameworkProjectionMode;
        EnableIsNotNullNarrowing = enableIsNotNullNarrowing;
        ExternalConstantValues = externalConstantValues ?? ImmutableDictionary<string, string>.Empty;
        EnabledAnalyzers = enabledAnalyzers ?? ImmutableHashSet<string>.Empty.WithComparer(StringComparer.OrdinalIgnoreCase);
        OptimizationLevel = optimizationLevel;
        SynthesizeStructuralToString = synthesizeStructuralToString;
        GeneratedCodeOptions = generatedCodeOptions ?? ImmutableDictionary<string, bool>.Empty.WithComparers(StringComparer.OrdinalIgnoreCase);
    }

    /// <summary>
    /// Gets explicit-only metadata import settings, or null for the default host-assisted import.
    /// </summary>
    public MetadataImportOptions? MetadataImportOptions { get; }

    /// <summary>Whether the target permits conversions between arrays of covariant reference elements.</summary>
    public bool AllowArrayCovariance { get; }

    public CompilationOptions WithAllowArrayCovariance(bool allow)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, allow);

    public RuntimePropagationContract? RuntimePropagationContract { get; }

    public CompilationOptions WithRuntimePropagationContract(RuntimePropagationContract? contract)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, contract, AllowArrayCovariance);

    public RuntimeIterationContract? RuntimeIterationContract { get; }

    public CompilationOptions WithRuntimeIterationContract(RuntimeIterationContract? contract)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, contract, RuntimePropagationContract, AllowArrayCovariance);


    public CompilationOptions WithMetadataImportOptions(MetadataImportOptions? options)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, options, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public OutputKind OutputKind { get; }

    /// <summary>
    /// Creates options with the specified output kind.
    /// </summary>
    public CompilationOptions WithOutputKind(OutputKind outputKind)
        => new(outputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public ImmutableDictionary<string, ReportDiagnostic> SpecificDiagnosticOptions { get; }

    public bool RunAnalyzers { get; }

    public ImmutableHashSet<string> DisabledAnalyzers { get; }

    public ImmutableHashSet<string> EnabledAnalyzers { get; }

    internal ImmutableDictionary<string, bool> GeneratedCodeOptions { get; }

    internal CompilationOptions WithGeneratedCodeOptions(ImmutableDictionary<string, bool> options)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, options, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    /// <summary>
    /// Gets the optimization policy used when lowering and emitting code.
    /// </summary>
    public OptimizationLevel OptimizationLevel { get; }

    /// <summary>
    /// Gets whether records and unions synthesize reflection-based <c>ToString</c>
    /// overrides. Restricted target frameworks can inherit the runtime default.
    /// </summary>
    public bool SynthesizeStructuralToString { get; }

    public CompilationOptions WithSynthesizeStructuralToString(bool enabled)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, enabled, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    /// <summary>
    /// Creates options with the specified optimization policy.
    /// </summary>
    public CompilationOptions WithOptimizationLevel(OptimizationLevel optimizationLevel)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, optimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public CompilationOptions WithSpecificDiagnosticOptions(IDictionary<string, ReportDiagnostic> options)
        => new(OutputKind, SpecificDiagnosticOptions.SetItems(options), RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public CompilationOptions WithExactSpecificDiagnosticOptions(ImmutableDictionary<string, ReportDiagnostic> options)
        => new(OutputKind, options, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public CompilationOptions WithSpecificDiagnosticOption(string diagnosticId, ReportDiagnostic option)
        => new(OutputKind, SpecificDiagnosticOptions.SetItem(diagnosticId, option), RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public CompilationOptions WithRunAnalyzers(bool runAnalyzers)
        => new(OutputKind, SpecificDiagnosticOptions, runAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public CompilationOptions WithDisabledAnalyzers(IEnumerable<string> disabledAnalyzers)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, AnalyzerOptionUtilities.ParseAnalyzerNameSet(disabledAnalyzers), PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public CompilationOptions WithEnabledAnalyzers(IEnumerable<string> enabledAnalyzers)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, AnalyzerOptionUtilities.ParseAnalyzerNameSet(enabledAnalyzers), OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public PerformanceInstrumentation PerformanceInstrumentation { get; }

    public CompilationOptions WithPerformanceInstrumentation(PerformanceInstrumentation? instrumentation)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, instrumentation ?? PerformanceInstrumentation.Disabled, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public ILoweringTraceSink? LoweringTrace { get; }

    public CompilationOptions WithLoweringTrace(ILoweringTraceSink? loweringTrace)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, loweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public AsyncInvestigationOptions AsyncInvestigation { get; }

    public CompilationOptions WithAsyncInvestigation(AsyncInvestigationOptions? asyncInvestigation)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, asyncInvestigation ?? AsyncInvestigationOptions.Disabled, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public IOverloadResolutionLogger? OverloadResolutionLogger { get; }

    public CompilationOptions WithOverloadResolutionLogger(IOverloadResolutionLogger? overloadResolutionLogger)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, overloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public bool EmbedCoreTypes { get; }

    public CompilationOptions WithEmbedCoreTypes(bool embedCoreTypes)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, embedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public bool AllowUnsafe { get; }

    public CompilationOptions WithAllowUnsafe(bool allowUnsafe)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, allowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public bool UseRuntimeAsync { get; }

    public CompilationOptions WithRuntimeAsync(bool useRuntimeAsync)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, useRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public bool AllowGlobalStatements { get; }

    public CompilationOptions WithAllowGlobalStatements(bool allowGlobalStatements)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, allowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public bool AllowNamespaceMembers { get; }

    public CompilationOptions WithAllowNamespaceMembers(bool allowNamespaceMembers)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, allowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public bool AllowNamespaceMemberImports { get; }

    public CompilationOptions WithAllowNamespaceMemberImports(bool allowNamespaceMemberImports)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, allowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public bool EnableSuggestions { get; }

    public CompilationOptions WithEnableSuggestions(bool enableSuggestions)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, enableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    private readonly ReturnedValueHandlingMode? _returnedValueHandlingMode;

    public ReturnedValueHandlingMode ReturnedValueHandlingMode => _returnedValueHandlingMode ?? ReturnedValueHandlingMode.Off;

    public bool ReturnedValueHandlingModeConfigured => _returnedValueHandlingMode.HasValue;

    public CompilationOptions WithReturnedValueHandlingMode(ReturnedValueHandlingMode mode)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, mode, FrameworkProjectionMode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public FrameworkProjectionMode FrameworkProjectionMode { get; }

    public CompilationOptions WithFrameworkProjectionMode(FrameworkProjectionMode mode)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, mode, EnableIsNotNullNarrowing, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    /// <summary>
    /// Gets whether direct <c>value is not null</c> checks narrow the value in
    /// the true branch. This compatibility feature is disabled by default and
    /// is intended primarily for consuming null-oriented interop contracts.
    /// </summary>
    public bool EnableIsNotNullNarrowing { get; }

    public CompilationOptions WithEnableIsNotNullNarrowing(bool enabled)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, enabled, ExternalConstantValues, EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);

    public ImmutableDictionary<string, string> ExternalConstantValues { get; }

    public CompilationOptions WithExternalConstantValues(IEnumerable<KeyValuePair<string, string>> values)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, DisabledAnalyzers, PerformanceInstrumentation, LoweringTrace, AsyncInvestigation, OverloadResolutionLogger, EmbedCoreTypes, AllowUnsafe, UseRuntimeAsync, AllowGlobalStatements, AllowNamespaceMembers, AllowNamespaceMemberImports, EnableSuggestions, _returnedValueHandlingMode, FrameworkProjectionMode, EnableIsNotNullNarrowing, values.ToImmutableDictionary(StringComparer.Ordinal), EnabledAnalyzers, OptimizationLevel, SynthesizeStructuralToString, GeneratedCodeOptions, MetadataImportOptions, RuntimeIterationContract, RuntimePropagationContract, AllowArrayCovariance);
}

public enum FrameworkProjectionMode
{
    None,
    Standard,
}

/// <summary>
/// Specifies whether compilation favors source-level debugging or optimized output.
/// </summary>
public enum OptimizationLevel
{
    Debug,
    Release,
}
