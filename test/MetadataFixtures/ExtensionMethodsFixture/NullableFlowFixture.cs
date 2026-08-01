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

    public static void SetName([NotNull] out string? value) => value = "raven";

    public static void SetDefault<T>([MaybeNull] out T value) => value = default;

    public static bool IsMissing([NotNullWhen(false)] string? value) => value is null;

    public static bool ArePresent(
        [NotNullWhen(true)] string? first,
        [NotNullWhen(true)] string? second) => first is not null && second is not null;

    public static bool MaybeClear(
        bool result,
        [MaybeNullWhen(true)] ref string? value)
    {
        if (result)
            value = null!;

        return result;
    }

    public static void AcceptNull([AllowNull] string value)
    {
    }

    public static void RejectNull([DisallowNull] string? value)
    {
    }

    public static void RejectOrdinaryNull(string value)
    {
    }

    [AllowNull]
    public static string RequiredName { get; set; } = string.Empty;

    [DisallowNull]
    public static string? OptionalName { get; set; }

    public static string OrdinaryName { get; set; } = string.Empty;
}

public sealed class MemberNullabilityFixture
{
    public string? Value { get; private set; }

    [MemberNotNull(nameof(Value))]
    public void Initialize() => Value = string.Empty;

    [MemberNotNullWhen(true, nameof(Value))]
    public bool EnsureValue(bool result)
    {
        if (result)
            Value = string.Empty;

        return result;
    }
}
