using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Metadata;

/// <summary>
/// Optional provider access to parameter facts without materializing a full signature.
/// Failed queries remain unavailable; consumers must not guess or force Parameters
/// as a fallback. Types are returned in this method's semantic context.
/// </summary>
internal interface IMethodParameterInfo
{
    bool TryGetParameterCount(out int count);
    bool TryGetParameterType(int index, [NotNullWhen(true)] out ITypeSymbol? type);
    bool TryGetParameterUsage(int index, out bool isOptional, out bool isVariadic);
}
