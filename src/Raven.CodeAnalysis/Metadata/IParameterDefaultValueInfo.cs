namespace Raven.CodeAnalysis.Metadata;

/// <summary>
/// Provider fact distinguishing a synthesized type default from a literal default.
/// Only meaningful when HasExplicitDefaultValue is true; the provider owns decoding.
/// </summary>
internal interface IParameterDefaultValueInfo
{
    bool ExplicitDefaultValueIsTypeDefault { get; }
}
