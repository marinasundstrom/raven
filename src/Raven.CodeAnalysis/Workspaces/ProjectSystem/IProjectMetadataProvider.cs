using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

/// <summary>Optional host adapter for an explicitly selected non-CLI project metadata format.</summary>
/// <remarks>The shared project system does not load target implementations. Hosts register an adapter;
/// projects without an explicit format continue using ordinary CLI references.</remarks>
public interface IProjectMetadataProvider
{
    /// <summary>Gets the value accepted for the evaluated RavenMetadataFormat property.</summary>
    string MetadataFormat { get; }

    /// <summary>Returns additional explicit configuration/artifact paths for file watching.</summary>
    IReadOnlyList<string> GetInputPaths(string projectFilePath, IReadOnlyDictionary<string, string> properties) => [];

    /// <summary>Loads explicit references and configures semantic options from evaluated project properties.</summary>
    /// <remarks>Paths are absolute. Properties are evaluated Raven-prefixed properties; relative target paths
    /// must be resolved against the project directory. Throw on invalid input; never substitute a different format.</remarks>
    ProjectMetadataConfiguration Load(string projectFilePath, string assemblyName, CompilationOptions options,
        IReadOnlyDictionary<string, string> properties, IReadOnlyList<string> referencePaths);
}

/// <summary>Immutable semantic project configuration returned by an explicit metadata provider.</summary>
public sealed record ProjectMetadataConfiguration(CompilationOptions Options, ImmutableArray<MetadataReference> References);
