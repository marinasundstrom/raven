namespace Raven.CodeAnalysis;

internal interface SequenceTypeServices
{
    TypeSymbol CharacterType { get; }
    NamedTypeSymbol? GetSequenceDefinition(bool asynchronous);
}
