using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Marks an assembly as a Raven compiler plugin and optionally declares one
/// of its macro plugin entry points.
/// </summary>
[AttributeUsage(AttributeTargets.Assembly, AllowMultiple = true)]
public sealed class RavenCompilerPluginAttribute : Attribute
{
    /// <summary>
    /// Marks the assembly and authorizes fallback discovery of its
    /// <see cref="IRavenMacroPlugin"/> implementations.
    /// </summary>
    public RavenCompilerPluginAttribute()
    {
    }

    /// <summary>
    /// Marks the assembly and declares a macro plugin entry point.
    /// Apply the attribute once for each entry point exported by the assembly.
    /// </summary>
    /// <param name="pluginType">
    /// A concrete type declared in the marked assembly that implements
    /// <see cref="IRavenMacroPlugin"/> and has a public parameterless
    /// constructor.
    /// </param>
    public RavenCompilerPluginAttribute(Type pluginType)
    {
        PluginType = pluginType ?? throw new ArgumentNullException(nameof(pluginType));
    }

    /// <summary>
    /// Gets the explicitly declared plugin entry point, or <see langword="null"/>
    /// when the assembly requested fallback discovery.
    /// </summary>
    public Type? PluginType { get; }
}
