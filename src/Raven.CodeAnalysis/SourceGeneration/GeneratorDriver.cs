using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public sealed class GeneratorDriver
{
    private static readonly DiagnosticDescriptor s_generatorFailure = DiagnosticDescriptor.Create(
        "RVNGEN001",
        "Source generator failure",
        "A source generator failed while producing source.",
        string.Empty,
        "Source generator '{0}' failed: {1}",
        "SourceGeneration",
        DiagnosticSeverity.Error);

    private readonly ImmutableArray<ISourceGenerator> _generators;
    private GeneratorDriverRunResult _runResult;

    private GeneratorDriver(ImmutableArray<ISourceGenerator> generators)
    {
        _generators = generators;
        _runResult = new GeneratorDriverRunResult([], [], []);
    }

    public static GeneratorDriver Create(params ISourceGenerator[] generators)
    {
        ArgumentNullException.ThrowIfNull(generators);
        if (generators.Any(static generator => generator is null))
            throw new ArgumentException("Generators cannot contain null.", nameof(generators));

        return new GeneratorDriver(generators.ToImmutableArray());
    }

    public GeneratorDriver RunGeneratorsAndUpdateCompilation(
        Compilation compilation,
        out Compilation outputCompilation,
        out ImmutableArray<Diagnostic> diagnostics,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(compilation);

        var results = ImmutableArray.CreateBuilder<GeneratorRunResult>(_generators.Length);
        var allSources = ImmutableArray.CreateBuilder<GeneratedSourceResult>();
        var allDiagnostics = ImmutableArray.CreateBuilder<Diagnostic>();

        foreach (var generator in _generators)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var generatorName = generator.GetType().FullName ?? generator.GetType().Name;
            var context = new GeneratorExecutionContext(compilation, cancellationToken);
            Exception? exception = null;

            try
            {
                generator.Initialize(new GeneratorInitializationContext(cancellationToken));
                generator.Execute(context);
            }
            catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
            {
                throw;
            }
            catch (Exception ex)
            {
                exception = ex;
                context.ReportDiagnostic(Diagnostic.Create(
                    s_generatorFailure,
                    Location.None,
                    generatorName,
                    ex.Message));
            }

            var sources = context.GetGeneratedSources(generatorName);
            var generatorDiagnostics = context.GetDiagnostics();
            allSources.AddRange(sources);
            allDiagnostics.AddRange(generatorDiagnostics);
            results.Add(new GeneratorRunResult(generator, sources, generatorDiagnostics, exception));
        }

        var generatedSources = allSources.ToImmutable();
        diagnostics = allDiagnostics.ToImmutable();
        outputCompilation = generatedSources.IsEmpty
            ? compilation
            : compilation.AddSyntaxTrees(generatedSources.Select(static source => source.SyntaxTree).ToArray());
        outputCompilation = outputCompilation.WithGeneratorDiagnostics(diagnostics);

        var driver = new GeneratorDriver(_generators);
        driver._runResult = new GeneratorDriverRunResult(results.ToImmutable(), diagnostics, generatedSources);
        return driver;
    }

    public GeneratorDriverRunResult GetRunResult() => _runResult;
}

public readonly record struct GeneratedSourceResult(
    string HintName,
    Text.SourceText SourceText,
    Syntax.SyntaxTree SyntaxTree);

public readonly record struct GeneratorRunResult(
    ISourceGenerator Generator,
    ImmutableArray<GeneratedSourceResult> GeneratedSources,
    ImmutableArray<Diagnostic> Diagnostics,
    Exception? Exception);

public readonly record struct GeneratorDriverRunResult(
    ImmutableArray<GeneratorRunResult> Results,
    ImmutableArray<Diagnostic> Diagnostics,
    ImmutableArray<GeneratedSourceResult> GeneratedSources);
