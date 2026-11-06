namespace Raven.CodeAnalysis.Symbols;

internal sealed class UnsupportedTupleSignatureType(Compilation compilation)
    : ErrorTypeSymbolImplementation(compilation, "invalid tuple name metadata", compilation.GlobalNamespace, [], []);
