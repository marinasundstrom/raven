using System.Diagnostics.CodeAnalysis;

namespace Raven.ExtensionMethodsFixture;

public static class NullableFlowFixture
{
    [return: MaybeNull]
    public static T FindOrDefault<T>() => default;

    public static bool IsMissing([NotNullWhen(false)] string? value) => value is null;
}
