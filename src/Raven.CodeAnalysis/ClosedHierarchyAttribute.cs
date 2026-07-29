namespace System.Runtime.CompilerServices;

/// <summary>
/// Carries the permitted direct subtypes of a closed hierarchy until the
/// runtime provides the well-known metadata contract.
/// </summary>
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Interface, Inherited = false)]
public sealed class ClosedHierarchyAttribute(params Type[] permittedTypes) : Attribute
{
    public Type[] PermittedTypes { get; } = permittedTypes;
}
