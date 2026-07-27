using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Marks a top-level declaration and its nested declarations as belonging to
/// the compile-time-only local macro partition.
/// </summary>
[AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = false)]
public sealed class LocalMacroAttribute : Attribute
{
}
