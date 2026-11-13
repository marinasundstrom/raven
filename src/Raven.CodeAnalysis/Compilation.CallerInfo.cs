using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation : CallerInfoValidationServices
{
    ImmutableArray<AttributeData> CallerInfoValidationServices.GetAttributesForUsageValidation(Symbol symbol)
        => symbol is SourceSymbol source
            ? source.GetAttributesForUsageValidation()
            : symbol.GetAttributes();
}
