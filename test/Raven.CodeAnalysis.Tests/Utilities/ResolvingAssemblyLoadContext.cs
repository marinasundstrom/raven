using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.Loader;

namespace Raven.CodeAnalysis.Tests.Utilities;

internal sealed class ResolvingAssemblyLoadContext : AssemblyLoadContext, IDisposable
{
    private readonly Dictionary<string, string> _pathsByFullName;
    private readonly Dictionary<string, string> _pathsBySimpleName;

    public ResolvingAssemblyLoadContext(IEnumerable<string> referencePaths)
        : base(isCollectible: true)
    {
        if (referencePaths is null)
            throw new ArgumentNullException(nameof(referencePaths));

        _pathsByFullName = new Dictionary<string, string>(StringComparer.Ordinal);
        _pathsBySimpleName = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);

        foreach (var path in referencePaths)
        {
            if (string.IsNullOrEmpty(path) || !File.Exists(path))
                continue;

            AssemblyName? identity = null;
            try
            {
                identity = AssemblyName.GetAssemblyName(path);
            }
            catch
            {
                // Skip files that are not valid assemblies.
                continue;
            }

            if (identity.FullName is { Length: > 0 } fullName && !_pathsByFullName.ContainsKey(fullName))
                _pathsByFullName[fullName] = path;

            if (!string.IsNullOrEmpty(identity.Name) && !_pathsBySimpleName.ContainsKey(identity.Name!))
                _pathsBySimpleName[identity.Name!] = path;
        }
    }

    protected override Assembly? Load(AssemblyName assemblyName)
    {
        if (assemblyName.FullName is { Length: > 0 } fullName && _pathsByFullName.TryGetValue(fullName, out var exactPath))
        {
            return LoadFromAssemblyPath(exactPath);
        }

        if (!string.IsNullOrEmpty(assemblyName.Name) && _pathsBySimpleName.TryGetValue(assemblyName.Name!, out var simplePath))
        {
            return LoadFromAssemblyPath(simplePath);
        }

        return null;
    }

    public void Dispose()
    {
        Unload();
    }
}
