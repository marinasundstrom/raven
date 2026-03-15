using System.Collections.Immutable;

using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis;

public static class BuiltInCodeRefactoringProviders
{
    public static ImmutableArray<CodeRefactoringProvider> CreateDefault()
    {
        return
        [
            new TargetTypedUnionCaseRefactoringProvider(),
            new SingleStatementBlockBodyRefactoringProvider(),
            new ExpressionBodyToBlockBodyRefactoringProvider(),
            new RedundantAccessorDeclarationRefactoringProvider(),
            new StringConcatenationRefactoringProvider(),
        ];
    }
}
