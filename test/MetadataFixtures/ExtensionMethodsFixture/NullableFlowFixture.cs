using System.Diagnostics.CodeAnalysis;

namespace Raven.ExtensionMethodsFixture;

public static class NullableFlowFixture
{
    [return: MaybeNull]
    public static T FindOrDefault<T>() => default;

    public static bool IsMissing([NotNullWhen(false)] string? value) => value is null;

    public static bool ArePresent(
        [NotNullWhen(true)] string? first,
        [NotNullWhen(true)] string? second) => first is not null && second is not null;
}
