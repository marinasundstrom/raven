using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Diagnostics;

public static class BuiltInCodeFixProviders
{
    public static ImmutableArray<CodeFixProvider> CreateDefault()
    {
        return
        [
            new MissingReturnTypeAnnotationCodeFixProvider(),
            new PreferTargetTypedUnionCaseCodeFixProvider(),
            new PreferTargetTypedUnionCaseInTargetTypedContextCodeFixProvider(),
            new PreferValInsteadOfLetCodeFixProvider(),
            new VarCanBeValCodeFixProvider(),
            new PreferNewLineBetweenDeclarationsCodeFixProvider(),
            new NonNullDeclarationsCodeFixProvider(),
            new PreferDuLinqExtensionsCodeFixProvider(),
            new PreferIsNullOverEqualityCodeFixProvider(),
        ];
}
}
