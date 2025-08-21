using System.Reflection;

using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis;

/// <summary>Represents a reference to one or more analyzers.</summary>
public class AnalyzerReference
{
    private readonly Func<IEnumerable<IDiagnosticAnalyzer>> _analyzerFactory;

    /// <summary>Create a reference from a specific analyzer instance.</summary>
    public AnalyzerReference(IDiagnosticAnalyzer analyzer)
        : this(() => [analyzer])
    {
    }

    /// <summary>Create a reference from an analyzer type.</summary>
    public AnalyzerReference(Type analyzerType)
        : this(() => [(IDiagnosticAnalyzer)Activator.CreateInstance(analyzerType)!])
    {
        if (!typeof(IDiagnosticAnalyzer).IsAssignableFrom(analyzerType))
            throw new ArgumentException("Type must implement IDiagnosticAnalyzer", nameof(analyzerType));
    }

    /// <summary>Create a reference from an assembly containing analyzers.</summary>
    public AnalyzerReference(Assembly assembly)
        : this(() =>
            assembly.GetTypes()
                .Where(t => typeof(IDiagnosticAnalyzer).IsAssignableFrom(t) && !t.IsAbstract && t.GetConstructor(Type.EmptyTypes) != null)
                .Select(t => (IDiagnosticAnalyzer)Activator.CreateInstance(t)!))
    {
    }

    private AnalyzerReference(Func<IEnumerable<IDiagnosticAnalyzer>> analyzerFactory)
    {
        _analyzerFactory = analyzerFactory ?? throw new ArgumentNullException(nameof(analyzerFactory));
    }

    internal IEnumerable<IDiagnosticAnalyzer> GetAnalyzers() => _analyzerFactory();
}
