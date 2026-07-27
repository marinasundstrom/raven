using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Marks an assembly as a Raven compiler plugin and optionally declares one
/// of its exported macro types.
/// </summary>
[AttributeUsage(AttributeTargets.Assembly, AllowMultiple = true)]
public sealed class RavenCompilerPluginAttribute : Attribute
{
    /// <summary>
    /// Marks the assembly and authorizes fallback discovery of its
    /// <see cref="IMacroDefinition"/> implementations.
    /// </summary>
    public RavenCompilerPluginAttribute()
    {
    }

    /// <summary>
    /// Marks the assembly and declares an exported macro definition.
    /// Apply the attribute once for each macro exported by the assembly.
    /// </summary>
    /// <param name="exportedType">
    /// A concrete type declared in the marked assembly that implements
    /// <see cref="IMacroDefinition"/> and
    /// has a public parameterless constructor.
    /// </param>
    public RavenCompilerPluginAttribute(Type exportedType)
    {
        ExportedType = exportedType ?? throw new ArgumentNullException(nameof(exportedType));
    }

    /// <summary>
    /// Gets the explicitly exported macro type, or
    /// <see langword="null"/> when the assembly requested fallback discovery.
    /// </summary>
    public Type? ExportedType { get; }
}
