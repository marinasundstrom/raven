using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation : AttributeValidationServices
{
    ImmutableArray<AttributeData> CallerInfoValidationServices.GetAttributesForUsageValidation(Symbol symbol)
        => symbol is SourceSymbol source
            ? source.GetAttributesForUsageValidation()
            : symbol.GetAttributes();

    bool AttributeValidationServices.IsPrimaryConstructor(Symbol symbol)
        => symbol is SourceMethodSymbol { MethodKind: MethodKind.Constructor, IsPrimaryConstructorSymbol: true };

    bool AttributeValidationServices.IsNamespaceContainer(Symbol symbol)
        => symbol is SynthesizedNamespaceMembersClassSymbol;
}
