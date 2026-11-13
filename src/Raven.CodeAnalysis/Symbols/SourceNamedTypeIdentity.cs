namespace Raven.CodeAnalysis.Symbols;

// Source declaration facts used by lookup and constructed-type substitution.
internal interface SourceNamedTypeIdentity : NamedTypeSymbol
{
    TypeSymbol? ExtensionReceiverType { get; }
    bool IsExtensionDeclaration { get; }
    bool HasMutableMemberShape { get; }
}
