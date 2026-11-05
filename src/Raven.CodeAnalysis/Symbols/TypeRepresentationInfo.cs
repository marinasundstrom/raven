using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

// Read-only representation facts used without depending on concrete symbol storage.
internal interface ILiteralTypeInfo
{
    TypeSymbol UnderlyingType { get; }
}

internal interface IByRefTypeInfo
{
    TypeSymbol ElementType { get; }
}

internal interface ISynthesizedDelegateSignature
{
    ImmutableArray<TypeSymbol> ParameterTypes { get; }
    ImmutableArray<RefKind> ParameterRefKinds { get; }
    TypeSymbol ReturnType { get; }
}
