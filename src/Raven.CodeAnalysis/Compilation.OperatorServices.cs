using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    TypeSymbol BinaryOperatorServices.CreateNullableType(TypeSymbol type) => type.GetNullableType();

    bool BinaryOperatorServices.HasRuntimeSelfContract => HasNativeSelfContract;

    MethodSymbol? BinaryOperatorServices.ProjectRuntimeSelfOperator(TypeSymbol receiver, MethodSymbol method)
        => HasNativeSelfContract && RuntimeSelfTypes.Contains(this, method)
            ? new NativeSelfMethodSymbol(this, receiver, method)
            : null;
}
