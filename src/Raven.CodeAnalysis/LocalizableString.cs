using System.Resources;

namespace Raven.CodeAnalysis;

public abstract class LocalizableString
{
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
}