namespace Raven.CodeAnalysis.Symbols;

/// <summary>A method's resolved override identity, when available.</summary>
internal interface MethodOverrideInfo
{
    MethodSymbol? OverriddenMethod { get; }
}
