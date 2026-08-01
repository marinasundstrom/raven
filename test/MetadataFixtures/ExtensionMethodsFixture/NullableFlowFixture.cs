using System.Diagnostics.CodeAnalysis;

namespace Raven.ExtensionMethodsFixture;

public static class NullableFlowFixture
{
    [return: MaybeNull]
    public static T FindOrDefault<T>() => default;

    [return: MaybeNull]
    public static string FindName() => null;

    [return: NotNullIfNotNull(nameof(value))]
    public static string? Echo(string? value) => value;

    public static bool IsMissing([NotNullWhen(false)] string? value) => value is null;

    public static bool ArePresent(
        [NotNullWhen(true)] string? first,
        [NotNullWhen(true)] string? second) => first is not null && second is not null;
}
