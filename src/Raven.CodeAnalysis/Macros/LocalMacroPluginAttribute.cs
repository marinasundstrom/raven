using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Marks a plugin declaration whose complete source file belongs to the
/// compile-time-only local macro partition.
/// </summary>
[AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = false)]
public sealed class LocalMacroPluginAttribute : Attribute
{
}
