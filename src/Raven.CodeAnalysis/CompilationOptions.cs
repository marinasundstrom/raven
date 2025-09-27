using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

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
        string? rootNamespace = null)
    {
        OutputKind = outputKind;
        SpecificDiagnosticOptions = specificDiagnosticOptions ?? ImmutableDictionary<string, ReportDiagnostic>.Empty;
        RunAnalyzers = runAnalyzers;
        RootNamespace = NormalizeRootNamespace(rootNamespace);
    }

    public OutputKind OutputKind { get; }

    public ImmutableDictionary<string, ReportDiagnostic> SpecificDiagnosticOptions { get; }

    public bool RunAnalyzers { get; }

    public string? RootNamespace { get; }

    public CompilationOptions WithSpecificDiagnosticOptions(IDictionary<string, ReportDiagnostic> options)
        => new(OutputKind, SpecificDiagnosticOptions.SetItems(options), RunAnalyzers, RootNamespace);

    public CompilationOptions WithSpecificDiagnosticOption(string diagnosticId, ReportDiagnostic option)
        => new(OutputKind, SpecificDiagnosticOptions.SetItem(diagnosticId, option), RunAnalyzers, RootNamespace);

    public CompilationOptions WithRunAnalyzers(bool runAnalyzers)
        => new(OutputKind, SpecificDiagnosticOptions, runAnalyzers, RootNamespace);

    public CompilationOptions WithRootNamespace(string? rootNamespace)
        => new(OutputKind, SpecificDiagnosticOptions, RunAnalyzers, rootNamespace);

    private static string? NormalizeRootNamespace(string? rootNamespace)
    {
        if (string.IsNullOrWhiteSpace(rootNamespace))
            return null;

        var parts = rootNamespace
            .Split('.', StringSplitOptions.RemoveEmptyEntries)
            .Select(part => part.Trim())
            .Where(part => part.Length > 0)
            .ToArray();

        return parts.Length == 0 ? null : string.Join('.', parts);
    }
}