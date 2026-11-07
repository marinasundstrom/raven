#if !NET11_0_OR_GREATER
namespace System.Runtime.CompilerServices;

[AttributeUsage(AttributeTargets.Class, Inherited = false)]
internal sealed class IsClosedTypeAttribute : Attribute
{
    public Type[] DerivedTypes { get; set; } = [];
}
#endif
