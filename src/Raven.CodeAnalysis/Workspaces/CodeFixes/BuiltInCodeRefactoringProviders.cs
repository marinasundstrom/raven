using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public static class BuiltInCodeRefactoringProviders
{
    public static ImmutableArray<CodeRefactoringProvider> CreateDefault() => [];
}
