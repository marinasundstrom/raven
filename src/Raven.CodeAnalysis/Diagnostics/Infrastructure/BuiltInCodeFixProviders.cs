using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Diagnostics;

public static class BuiltInCodeFixProviders
{
    public static ImmutableArray<CodeFixProvider> CreateDefault()
    {
        return
        [
            new MissingReturnTypeAnnotationCodeFixProvider(),
            new PreferLetInsteadOfValCodeFixProvider(),
            new VarCanBeLetCodeFixProvider(),
            new MemberCanBePrivateCodeFixProvider(),
            new MemberCanBeStaticCodeFixProvider(),
            new UnusedPropertyCodeFixProvider(),
            new PreferNewLineBetweenDeclarationsCodeFixProvider(),
            new PreferOptionOverNullableCodeFixProvider(),
            new PreferDuLinqExtensionsCodeFixProvider(),
            new PreferIsNullOverEqualityCodeFixProvider(),
            new ConstructorParameterNamingCodeFixProvider(),
            new ConversionCastCodeFixProvider(),
            new MatchExhaustivenessCodeFixProvider(),
            new RemoveRedundantImportCodeFixProvider(),
            new PragmaWarningSuppressionCodeFixProvider(),
        ];
    }
}
