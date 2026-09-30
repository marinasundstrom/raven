namespace Raven.CodeAnalysis.Metadata;

/// <summary>
/// Resolves provider-encoded extension receivers in a member's semantic context.
/// The member can be a constructed view of a declaration owned by this provider.
/// Encoding-specific generic parameter mapping belongs to the provider.
/// </summary>
internal interface IExtensionReceiverResolver
{
    ITypeSymbol? GetExtensionReceiverType(IMethodSymbol method);
    ITypeSymbol? GetExtensionReceiverType(IPropertySymbol property);
}
