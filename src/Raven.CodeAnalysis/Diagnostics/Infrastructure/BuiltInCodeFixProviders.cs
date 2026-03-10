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
            new MemberCanBePrivateCodeFixProvider(),
            new MemberCanBeStaticCodeFixProvider(),
            new UnusedPropertyCodeFixProvider(),
            new PreferNewLineBetweenDeclarationsCodeFixProvider(),
            new NonNullDeclarationsCodeFixProvider(),
            new PreferDuLinqExtensionsCodeFixProvider(),
            new PreferIsNullOverEqualityCodeFixProvider(),
            new RedundantAccessorDeclarationCodeFixProvider(),
            new StringConcatenationToInterpolatedStringCodeFixProvider(),
            new MergeStringLiteralConcatenationCodeFixProvider(),
            new ConstructorParameterNamingCodeFixProvider(),
            new ConversionCastCodeFixProvider(),
            new SingleStatementBlockBodyCodeFixProvider(),
        ];
}
}
