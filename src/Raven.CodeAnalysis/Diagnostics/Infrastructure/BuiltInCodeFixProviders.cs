using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Diagnostics;

public static class BuiltInCodeFixProviders
{
    public static ImmutableArray<CodeFixProvider> CreateDefault()
    {
        return
        [
            new ImplementInterfaceMembersCodeFixProvider(),
            new MissingReturnTypeAnnotationCodeFixProvider(),
            new VarCanBeLetCodeFixProvider(),
            new MemberCanBePrivateCodeFixProvider(),
            new MemberCanBeStaticCodeFixProvider(),
            new UnusedPropertyCodeFixProvider(),
            new PreferOptionOverNullableCodeFixProvider(),
            new PreferDuLinqExtensionsCodeFixProvider(),
            new PreferIsNullOverEqualityCodeFixProvider(),
            new ConversionCastCodeFixProvider(),
            new MatchExhaustivenessCodeFixProvider(),
            new RemoveRedundantImportCodeFixProvider(),
            new PragmaWarningSuppressionCodeFixProvider(),
        ];
    }
}
