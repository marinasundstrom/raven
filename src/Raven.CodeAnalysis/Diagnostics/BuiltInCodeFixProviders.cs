using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Diagnostics;

public static class BuiltInCodeFixProviders
{
    public static ImmutableArray<CodeFixProvider> CreateDefault()
    {
        return
        [
            new MissingReturnTypeAnnotationCodeFixProvider(),
            new PreferValInsteadOfLetCodeFixProvider(),
            new VarCanBeValCodeFixProvider(),
            new PreferNewLineBetweenDeclarationsCodeFixProvider(),
        ];
    }
}
