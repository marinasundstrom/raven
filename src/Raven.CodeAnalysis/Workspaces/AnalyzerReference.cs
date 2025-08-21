using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace Raven.CodeAnalysis;

/// <summary>Represents a reference to one or more analyzers.</summary>
public class AnalyzerReference
{
    private readonly Func<IEnumerable<IRavenAnalyzer>> _analyzerFactory;

    /// <summary>Create a reference from a specific analyzer instance.</summary>
    public AnalyzerReference(IRavenAnalyzer analyzer)
        : this(() => new[] { analyzer })
    {
    }

    /// <summary>Create a reference from an analyzer type.</summary>
    public AnalyzerReference(Type analyzerType)
        : this(() => new[] { (IRavenAnalyzer)Activator.CreateInstance(analyzerType)! })
    {
        if (!typeof(IRavenAnalyzer).IsAssignableFrom(analyzerType))
            throw new ArgumentException("Type must implement IRavenAnalyzer", nameof(analyzerType));
    }

    /// <summary>Create a reference from an assembly containing analyzers.</summary>
    public AnalyzerReference(Assembly assembly)
        : this(() =>
            assembly.GetTypes()
                .Where(t => typeof(IRavenAnalyzer).IsAssignableFrom(t) && !t.IsAbstract && t.GetConstructor(Type.EmptyTypes) != null)
                .Select(t => (IRavenAnalyzer)Activator.CreateInstance(t)!))
    {
    }

    private AnalyzerReference(Func<IEnumerable<IRavenAnalyzer>> analyzerFactory)
    {
        _analyzerFactory = analyzerFactory ?? throw new ArgumentNullException(nameof(analyzerFactory));
    }

    internal IEnumerable<IRavenAnalyzer> GetAnalyzers() => _analyzerFactory();
}
