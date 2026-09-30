namespace Raven.CodeAnalysis.Metadata;

/// <summary>
/// Optional provider shortcut for classifying an argument against an encoded
/// parameter without resolving the complete signature. False means unavailable:
/// callers must use normal symbol conversion. A successful None is a rejection.
/// This capability does not replace Compilation.ClassifyConversion.
/// </summary>
internal interface IParameterConversionClassifier
{
    bool TryClassifyParameterConversion(ITypeSymbol argumentType, int parameterIndex, out ParameterConversionKind conversion);
}

internal enum ParameterConversionKind
{
    None,
    Identity,
    ImplicitNumeric,
    ToObject
}
