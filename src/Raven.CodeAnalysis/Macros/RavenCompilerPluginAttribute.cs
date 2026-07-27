using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Marks an assembly as a Raven compiler plugin whose macro entry points may
/// be discovered by the compiler.
/// </summary>
[AttributeUsage(AttributeTargets.Assembly, AllowMultiple = false)]
public sealed class RavenCompilerPluginAttribute : Attribute;
