namespace Raven.CodeAnalysis.Symbols;

// A source member's signature placeholder can be replaced by its bound declaration.
internal interface SourceMethodDeclarationIdentity : MethodSymbol
{
    bool IsSignatureSkeleton { get; }
}
