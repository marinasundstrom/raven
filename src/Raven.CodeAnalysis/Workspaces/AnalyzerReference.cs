using System.Reflection;

using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis;

/// <summary>Represents a reference to one or more analyzers.</summary>
public class AnalyzerReference
{
    private readonly Func<IEnumerable<DiagnosticAnalyzer>> _analyzerFactory;

    /// <summary>Create a reference from a specific analyzer instance.</summary>
    public AnalyzerReference(DiagnosticAnalyzer analyzer)
        : this(() => [analyzer])
    {
    }

    /// <summary>Create a reference from an analyzer type.</summary>
    public AnalyzerReference(Type analyzerType)
        : this(() => [(DiagnosticAnalyzer)Activator.CreateInstance(analyzerType)!])
    {
        if (!typeof(DiagnosticAnalyzer).IsAssignableFrom(analyzerType))
            throw new ArgumentException("Type must implement DiagnosticAnalyzer", nameof(analyzerType));
    }

    /// <summary>Create a reference from an assembly containing analyzers.</summary>
    public AnalyzerReference(Assembly assembly)
        : this(() =>
            assembly.GetTypes()
                .Where(t => typeof(DiagnosticAnalyzer).IsAssignableFrom(t) && !t.IsAbstract && t.GetConstructor(Type.EmptyTypes) != null)
                .Select(t => (DiagnosticAnalyzer)Activator.CreateInstance(t)!))
    {
    }

    private AnalyzerReference(Func<IEnumerable<DiagnosticAnalyzer>> analyzerFactory)
    {
        _analyzerFactory = analyzerFactory ?? throw new ArgumentNullException(nameof(analyzerFactory));
    }

    internal IEnumerable<DiagnosticAnalyzer> GetAnalyzers() => _analyzerFactory();
}
