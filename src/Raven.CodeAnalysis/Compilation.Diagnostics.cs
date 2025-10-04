using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    public ImmutableArray<Diagnostic> GetDiagnostics(CompilationWithAnalyzersOptions? analyzerOptions = null, CancellationToken cancellationToken = default)
    {
        var diagnostics = new List<Diagnostic>();

        EnsureSetup();

        foreach (var syntaxTree in SyntaxTrees)
        {
            foreach (var diagnostic in syntaxTree.GetDiagnostics(cancellationToken))
                Add(diagnostic);

            var model = GetSemanticModel(syntaxTree);
            foreach (var diagnostic in model.GetDiagnostics(cancellationToken))
                Add(diagnostic);
        }

        var entryPointDiagnostics = GetEntryPointDiagnostics(cancellationToken);
        foreach (var diagnostic in entryPointDiagnostics)
            Add(diagnostic);

        if (Options.OutputKind == OutputKind.ConsoleApplication
            && entryPointDiagnostics.IsDefaultOrEmpty
            && GetEntryPoint(cancellationToken) is null)
        {
            Add(Diagnostic.Create(CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint, Location.None));
        }

        return diagnostics.OrderBy(x => x.Location).ToImmutableArray();

        void Add(Diagnostic diagnostic)
        {
            var mapped = ApplyCompilationOptions(diagnostic, analyzerOptions?.ReportSuppressedDiagnostics ?? false);
            if (mapped is not null)
                diagnostics.Add(mapped);
        }
    }

    internal Diagnostic? ApplyCompilationOptions(Diagnostic diagnostic, bool reportSuppressedDiagnostics = false)
    {
        if (Options.SpecificDiagnosticOptions.TryGetValue(diagnostic.Descriptor.Id, out var report))
        {
            if (report == ReportDiagnostic.Suppress)
                return reportSuppressedDiagnostics ? diagnostic.WithSuppression(true) : null;

            if (report != ReportDiagnostic.Default)
            {
                var severity = report switch
                {
                    ReportDiagnostic.Error => DiagnosticSeverity.Error,
                    ReportDiagnostic.Warn => DiagnosticSeverity.Warning,
                    ReportDiagnostic.Info => DiagnosticSeverity.Info,
                    ReportDiagnostic.Hidden => DiagnosticSeverity.Hidden,
                    _ => diagnostic.Severity
                };

                if (severity != diagnostic.Severity)
                    return diagnostic.WithSeverity(severity);
            }
        }

        return diagnostic;
    }
}
