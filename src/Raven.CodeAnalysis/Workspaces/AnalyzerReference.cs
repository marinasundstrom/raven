using System;
using System.Linq;
using System.Reflection;
using System.Threading;

using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis;

/// <summary>Represents a reference to one or more analyzers.</summary>
public class AnalyzerReference
{
    private readonly Func<IEnumerable<DiagnosticAnalyzer>> _analyzerFactory;
    private readonly Func<IEnumerable<CodeFixProvider>> _codeFixProviderFactory;

    /// <summary>Create a reference from a specific analyzer instance.</summary>
    public AnalyzerReference(DiagnosticAnalyzer analyzer)
        : this(() => [analyzer], static () => [])
    {
    }

    /// <summary>Create a reference from an analyzer type.</summary>
    public AnalyzerReference(Type analyzerType)
        : this(() => [(DiagnosticAnalyzer)Activator.CreateInstance(analyzerType)!], static () => [])
    {
        if (!typeof(DiagnosticAnalyzer).IsAssignableFrom(analyzerType))
            throw new ArgumentException("Type must implement DiagnosticAnalyzer", nameof(analyzerType));
    }

    /// <summary>Create a reference from an assembly containing analyzers.</summary>
    public AnalyzerReference(Assembly assembly)
        : this(CreateAssemblyAnalyzerFactory(assembly), CreateAssemblyCodeFixProviderFactory(assembly))
    {
    }

    private AnalyzerReference(
        Func<IEnumerable<DiagnosticAnalyzer>> analyzerFactory,
        Func<IEnumerable<CodeFixProvider>> codeFixProviderFactory)
    {
        _analyzerFactory = analyzerFactory ?? throw new ArgumentNullException(nameof(analyzerFactory));
        _codeFixProviderFactory = codeFixProviderFactory ?? throw new ArgumentNullException(nameof(codeFixProviderFactory));
    }

    internal IEnumerable<DiagnosticAnalyzer> GetAnalyzers() => _analyzerFactory();

    internal IEnumerable<CodeFixProvider> GetCodeFixProviders() => _codeFixProviderFactory();

    private static Func<IEnumerable<DiagnosticAnalyzer>> CreateAssemblyAnalyzerFactory(Assembly assembly)
    {
        var analyzerTypes = new Lazy<Type[]>(
            () => GetLoadableTypes(assembly)
                .Where(t => typeof(DiagnosticAnalyzer).IsAssignableFrom(t) && !t.IsAbstract && t.GetConstructor(Type.EmptyTypes) != null)
                .OrderBy(static t => t.FullName, StringComparer.Ordinal)
                .ToArray(),
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => analyzerTypes.Value.Select(t => (DiagnosticAnalyzer)Activator.CreateInstance(t)!);
    }

    private static Func<IEnumerable<CodeFixProvider>> CreateAssemblyCodeFixProviderFactory(Assembly assembly)
    {
        var providerTypes = new Lazy<Type[]>(
            () => GetLoadableTypes(assembly)
                .Where(t => typeof(CodeFixProvider).IsAssignableFrom(t) && !t.IsAbstract && t.GetConstructor(Type.EmptyTypes) != null)
                .OrderBy(static t => t.FullName, StringComparer.Ordinal)
                .ToArray(),
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => providerTypes.Value.Select(t => (CodeFixProvider)Activator.CreateInstance(t)!);
    }

    private static IEnumerable<Type> GetLoadableTypes(Assembly assembly)
    {
        try
        {
            return assembly.GetTypes();
        }
        catch (ReflectionTypeLoadException exception)
        {
            return exception.Types.OfType<Type>();
        }
    }
}
