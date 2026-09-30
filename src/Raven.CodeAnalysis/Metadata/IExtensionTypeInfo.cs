namespace Raven.CodeAnalysis.Metadata;

/// <summary>
/// Provider-owned type-level extension discovery facts. These do not establish
/// applicability of any particular member or require a CLI attribute encoding.
/// </summary>
internal interface IExtensionTypeInfo
{
    ITypeSymbol? ExtensionReceiverType { get; }
    bool HasMemberLevelExtensions { get; }
}
