using System.Globalization;
using System.Resources;
using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis;

public abstract class LocalizableString : IEquatable<LocalizableString>
{
    public bool Equals(LocalizableString? other) => Equals((object?)other);

    public override bool Equals(object? obj) => ReferenceEquals(this, obj);

    public override int GetHashCode() => RuntimeHelpers.GetHashCode(this);

    public static bool operator ==(LocalizableString? left, LocalizableString? right) =>
        ReferenceEquals(left, right) || (left is not null && left.Equals(right));

    public static bool operator !=(LocalizableString? left, LocalizableString? right) => !(left == right);

    public static implicit operator LocalizableString(string value)
    {
        return new LocalizableStringImpl(value);
    }

    public static implicit operator string(LocalizableString value)
    {
        return value.ToString()!;
    }
}

public class LocalizableStringImpl : LocalizableString
{
    private readonly string _value;

    public LocalizableStringImpl(string value)
    {
        _value = value;
    }

    public override string ToString()
    {
        return _value;
    }

    public override bool Equals(object? obj) => obj is LocalizableStringImpl other &&
        GetType() == other.GetType() && _value == other._value;

    public override int GetHashCode() => HashCode.Combine(GetType(), _value);
}

public class LocalizableResourceString : LocalizableString
{
    private readonly string _nameOfLocalizableResource;
    private readonly ResourceManager _resourceManager;
    private readonly Type _resourceSource;

    public LocalizableResourceString(string nameOfLocalizableResource, System.Resources.ResourceManager resourceManager, Type resourceSource)
    {
        _nameOfLocalizableResource = nameOfLocalizableResource;
        _resourceManager = resourceManager;
        _resourceSource = resourceSource;
    }

    public override string ToString() =>
        _resourceManager.GetString(_nameOfLocalizableResource, CultureInfo.CurrentUICulture) ?? string.Empty;

    public override bool Equals(object? obj) => obj is LocalizableResourceString other &&
        GetType() == other.GetType() && _nameOfLocalizableResource == other._nameOfLocalizableResource &&
        ReferenceEquals(_resourceManager, other._resourceManager) && _resourceSource == other._resourceSource;

    public override int GetHashCode() => HashCode.Combine(
        GetType(), _nameOfLocalizableResource, RuntimeHelpers.GetHashCode(_resourceManager), _resourceSource);
}
