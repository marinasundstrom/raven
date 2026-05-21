namespace Raven.CodeAnalysis;

public enum ReturnedValueHandlingMode
{
    Off,
    Full
}

public static class ReturnedValueHandlingOptions
{
    public static bool TryParse(string? value, out ReturnedValueHandlingMode mode)
    {
        mode = default;
        if (string.IsNullOrWhiteSpace(value))
            return false;

        switch (value.Trim().ToLowerInvariant())
        {
            case "default":
            case "none":
            case "off":
            case "disabled":
            case "disable":
            case "false":
                mode = ReturnedValueHandlingMode.Off;
                return true;
            case "full":
            case "enabled":
            case "enable":
            case "true":
                mode = ReturnedValueHandlingMode.Full;
                return true;
            default:
                return false;
        }
    }

    public static string ToProjectFileValue(ReturnedValueHandlingMode mode)
        => mode switch
        {
            ReturnedValueHandlingMode.Full => "full",
            _ => "off"
        };
}
